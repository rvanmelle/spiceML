open Solver
open Recorder
open Gsl_helpers
open Printf

let info = Circuit.info

module TranAnalysis = functor (Solver: SOLVER) -> struct

  type t = { name : string; duration : float }

  (* RK8PD, RK4, RKF45, RKCK, RK2IMP, RK4IMP  -> 
     RK2 -> 1s
     GEAR1 -> 60s
  *)
  let stepper = Gsl_odeiv.RK2
  let reltol = 1e-9
  let abstol = 1e-12
  let step_size = 1e-10
      
  let create name duration =
    { name = name; duration = duration }
      
  let run tran ~xinit (s:Solver.t) (r:recorder) =
    info "Starting transient analysis...";
    let size = Solver.size s in
    let temp_v = V.create size in

    let c_matrix = `M (Solver.get_c_matrix s) in
    
    let func t x y =
      let x_v = V.of_array x in
      (*printf "time=%g\n" t;
      print_vector "x_v" x_v;
      print_vector "b_vec" (Solver.get_b_vec s (`DC t));*)
      let y_v = V.create ~init:0. size in
      Solver.apply_nonlinear_functions s ~x:x_v ~y:y_v;
      V.scale y_v (-.1.);
      (*print_vector "y_v1" y_v;*)
      V.add y_v (Solver.get_b_vec s (`DC t));
      ((Solver.get_a_matrix s) |*> x_v) temp_v;
      V.sub y_v temp_v;
      (*print_vector "temp_v" temp_v;
      print_vector "y_v3" y_v;*)
      (*print_matrix "c" (Solver.get_c_matrix s);*)
      (*let c_inverse = 
	match Gsl_linalg.invert_LU (`M (Solver.get_c_matrix s)) with
	  | `M foo -> foo
	  | `MF foo -> failwith "shitty"
      in*)
      let soln = Gsl_linalg.solve_LU ~protect:true c_matrix (`V y_v) in
      (*print_array "soln" soln;*)
      Array.blit soln 0 y 0 (Array.length soln)

      (*print_matrix "c_inverse" c_inverse;
      (c_inverse |*> y_v) y_v;
      print_vector "soln" y_v;
      Array.blit (V.to_array y_v) 0 y 0 (V.length y_v)*)
      
    and jac t x dfdx dfdt =
      printf "jacobian evaluated\n%!";
      let x_v = V.of_array x in
      (*M.memcpy ~src:(Solver.get_a_matrix s) ~dst:dfdx;
	Solver.apply_nonlinear_derivatives s ~x:x_v ~j:dfdx;*)
      let dfdt_v = Solver.get_b_vec s (`DC_DERIV t) in
      print_vector "dfdt_v" dfdt_v;
      Array.blit (V.to_array dfdt_v) 0 dfdt 0 (Array.length dfdt)
    in

    (* Setup the transient ode stepper *)
    let step    = Gsl_odeiv.make_step stepper ~dim:size in
    let control = Gsl_odeiv.make_control_yp_new
      ~eps_abs:abstol ~eps_rel:reltol in
    let evolve  = Gsl_odeiv.make_evolve size in
    
    let system = Gsl_odeiv.make_system func ~jac size in
    let (t, t1, h, y) = (0., tran.duration, step_size, xinit) in

    let start_time = Unix.gettimeofday () in
    let num_steps = ref 0 in
    let rec loop t h =
      incr num_steps;
      if (!num_steps mod 10000) = 0 then printf ".%!";
      if t < t1 then begin
	let (t, h) =
	  (*printf "go... t=%g h=%g\n%!" t h;*)
	  Gsl_odeiv.evolve_apply evolve control step system
	    ~t ~t1 ~h ~y
	in
	(*printf "tran t=%g\n%!" t; *)
	r#append_real tran.name t (fun i -> y.(i));
	loop t h
      end 
    in
    loop t h;
    let duration = (Unix.gettimeofday ()) -. start_time in
    info "TRAN: finished (steps=%d time=%g)" !num_steps duration
    
end;;
