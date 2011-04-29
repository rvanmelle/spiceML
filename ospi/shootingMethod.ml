open Solver
open Recorder
open Gsl_helpers
open Printf

let info = Circuit.info
let wall_time = Unix.gettimeofday
(*
  This module implements the Newton Shooting Method for solving
  steady-state solutions of non-linear circuits with a periodic input.
  This is particularly well-suited to solutions with inputs that
  exhibit sharp non-linearities.

  - we are looking for an initial condition x_o such that
  phi(x_0, 0, T) - x_0 = 0     *where*
  T = the period of the input
  x_0 = our guess at the initial condition
*)
  
module ShootingMethod = functor (Solver: SOLVER) -> struct

  exception SingularMatrix
  exception UnexpectedBehavior
  type t = { name : string;
	     period : float }

  type jacobian = BACKWARD_EULER | TRAPEZOIDAL
      
  (* Settings for the transient ODE stepper algorithm *)
  let stepper = Gsl_odeiv.RK2
  let reltol = 1e-9
  let abstol = 1e-12
  let step_size = 1e-10

  (* Settings for the multi-dimensional root-solver *)
  let root_solver = Gsl_multiroot.Deriv.NEWTON
  let sm_jacobian = BACKWARD_EULER
  let max_iter = 200
  let eps_abs = 1e-7
    
  let create name period = { name = name; period = period }

  (* Main routine for actually running the analysis *)
  let run sm ~xinit (s:Solver.t) (r:recorder) =
    info "Starting Newton Shooting Method analysis...";

    (* Allocate a bunch of matrices which are reused throughout *)
    let size = Solver.size s in
    let temp_v = V.create size in
    let temp_m = M.create size size in
    let temp_m2 = M.create size size in
    let y_v = V.create size in
    let c_matrix = Solver.get_c_matrix s in
    let g_matrix = Solver.get_a_matrix s in

    let run_transient ?(periods=1) ?(save=false) xinit =
      (* Runs a transient analysis starting with xinit for a period of
	 sm.period. Returns the (xfinal, d_phi / d_x0) *)
      let start_t = wall_time () in
      let x_start = V.of_array xinit in
      let ode_func t x y =
	let x_v = V.of_array x in
	V.set_zero y_v;
	Solver.apply_nonlinear_functions s ~x:x_v ~y:y_v;
	V.scale y_v (-.1.);
	V.add y_v (Solver.get_b_vec s (`DC t));
	(g_matrix |*> x_v) temp_v;
	V.sub y_v temp_v;
	let soln = Gsl_linalg.solve_LU ~protect:true (`M c_matrix) (`V y_v) in
	Array.blit soln 0 y 0 (Array.length soln)
      in

      (* Create the ODE solver stepper, evolver, etc. *)
      let step    = Gsl_odeiv.make_step stepper ~dim:size in
      let control = Gsl_odeiv.make_control_yp_new
	~eps_abs:abstol ~eps_rel:reltol in
      let evolve  = Gsl_odeiv.make_evolve size in
      let system  = Gsl_odeiv.make_system ode_func size in
      let (t, t1, h, y) = (0., (sm.period *. (float periods)), step_size, xinit) in

      let num_steps = ref 0 in
      let cur_j = M.create size size in

      (* Trapezoidal based jacobian calculation function *)
      let jac_func_tr h x =
	(* WARNING: this is not yet right --> DO NOT USE *)
	let x_v = V.of_array x in
	V.set_zero y_v;
	Solver.apply_nonlinear_derivatives s ~x:x_v ~j:temp_m;
	M.add temp_m g_matrix;
	M.scale temp_m (h /. 2.);
	let lhs_m = M.copy c_matrix and rhs_m = M.copy c_matrix in
	M.add lhs_m temp_m;
	M.sub rhs_m temp_m;
	let lhs_inverse = Gsl_linalg.invert_LU (`M lhs_m) in
	let lhs_inverse = match lhs_inverse with
	  | `M x -> x
	  | _ -> raise SingularMatrix
	in
	(lhs_inverse |*| rhs_m) temp_m2;
	(temp_m2 |*| cur_j) temp_m;
	M.memcpy ~src:temp_m ~dst:cur_j
      in

      (* Backward euler-based jacobian calculation function *)
      let jac_func_be h x =
	let x_v = V.of_array x in
	V.set_zero y_v;
	Solver.apply_nonlinear_derivatives s ~x:x_v ~j:temp_m;
	M.add temp_m g_matrix;
	M.scale temp_m h;
	M.add temp_m c_matrix;
	let lhs_inverse = match Gsl_linalg.invert_LU (`M temp_m) with
	  | `M x -> x
	  | _ -> raise SingularMatrix
	in
	let rhs_m = M.copy c_matrix in
	(lhs_inverse |*| rhs_m) temp_m2;
	(temp_m2 |*| cur_j) temp_m;
	M.memcpy ~src:temp_m ~dst:cur_j
      in

      (* This is where we actually drive the transient analysis to completion *)
      M.set_id cur_j;
      let rec loop t h =
	incr num_steps;
	if (!num_steps mod 10000) = 0 then printf ".%!";
	if t < t1 then begin
	  let (t, h) =
	    (* Update the jacobian *)
	    let () = match sm_jacobian with
	      | BACKWARD_EULER -> jac_func_be h y
	      | TRAPEZOIDAL -> jac_func_tr h y
	    in
	    Gsl_odeiv.evolve_apply evolve control step system ~t ~t1 ~h ~y
	  in
	  if save then r#append_real sm.name t (fun i -> y.(i));
	  loop t h
	end 
      in
      loop t h;
      info " tran --> time=%g steps=%d" ((wall_time ()) -. start_t) !num_steps;
      let y_v = V.of_array y in
      print_vector "xstart" x_start;
      print_array "xfinal" y;
      V.sub y_v x_start;
      print_vector"error vec" y_v;
      y_v, cur_j
    in
    
    let start_time = Unix.gettimeofday () in

    (* This is the non-linear *newton* part of the shooting method *)
    let maxiter=max_iter
    and epsabs=eps_abs
    and solver_method = root_solver in  (* HYBRIDSJ *)
    let m_ident = M.create size size in
    M.set_id m_ident;
    let fdf ~x ~f ~j =
      let y, cur_j = run_transient (V.to_array x) in
      V.memcpy ~src:y ~dst:f;
      M.memcpy ~src:cur_j ~dst:j;
      M.sub j m_ident;
    in
    let gf = {
      Gsl_fun.multi_f = (fun ~x ~f -> raise UnexpectedBehavior);
      Gsl_fun.multi_df = (fun ~x ~j -> raise UnexpectedBehavior);
      Gsl_fun.multi_fdf = fdf; }
    in
    let solv = Gsl_multiroot.Deriv.make solver_method size gf xinit in
    let root_solver_steps = ref 0 in
    let rec proc iter =
      incr root_solver_steps;
      Gsl_multiroot.Deriv.iterate solv;
      let status = Gsl_multiroot.Deriv.test_residual solv epsabs in
      match status with
        | true -> Converged, iter
        | false when iter >= maxiter -> IterationLimitExceeded, iter
        | false -> proc (succ iter)
    in
    let status, iters = proc 1 in
    Gsl_multiroot.Deriv.get_state solv ~x:xinit ();
    
    (* Calculate total time and report the result *)
    let duration = (Unix.gettimeofday ()) -. start_time in
    ignore (run_transient ~periods:2 ~save:true (V.to_array xinit));

    (*  Report results *)
    match status with
      | Converged ->
	  info "Newton Shooting Method: finished (steps=%d time=%g)"
	    !root_solver_steps duration
      | IterationLimitExceeded ->
	  info "FAILURE: Newton Shooting Method (steps=%d time=%g)"
	    !root_solver_steps duration
    
end;;
