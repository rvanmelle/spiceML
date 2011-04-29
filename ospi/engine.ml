open Printf
open Solver
open Recorder
open Complex
open Gsl_helpers

let debug = Circuit.debug
let info = Circuit.info
let warn = Circuit.warn

module DcAnalysis = functor (Solver: SOLVER) -> struct

  let stepping = true
  let solver_type = `DERIV
  exception ConvergenceFailed of string
    
  let print_status result alpha = 
    match result with
      | Converged ->
	  debug "converged at alpha=%g" alpha;
      | IterationLimitExceeded ->
	  debug "failed at alpha=%g" alpha

  let solve_at (s:Solver.t) (r:recorder) source_val =
    if stepping then begin
      info "DC: starting source stepping ";
      let num_steps = 50 in
      let init_x = Array.make (Solver.size s) 0. in
      for i = 0 to num_steps do
	printf ".";
	let alpha = (float i) /. (float num_steps) in
	Solver.set_b_vec ~alpha s source_val;
	let result = match solver_type with
	  | `DERIV -> Solver.solve_nonlinear ~init_x s
	  | `NODERIV -> Solver.solve_nonlinear_noderiv ~init_x s
	in
	(match result with
	  | Converged ->
	      let soln = Solver.get_solution s in
	      Array.blit soln 0 init_x 0 (Solver.size s);
	  | IterationLimitExceeded ->
	      let s = sprintf "DC source stepping failed at alpha=%g\n%!" alpha in
	      raise (ConvergenceFailed s)
	);
      done;
      info "\nDC: finished!";
    end else begin
      Solver.set_b_vec s source_val;
      let result = match solver_type with
	| `DERIV -> Solver.solve_nonlinear s
	| `NODERIV -> Solver.solve_nonlinear_noderiv s
      in
      print_status result 1.;
    end
      
  let solve ?(name="dc") ?source_val (s:Solver.t) (r:recorder) =
    let source_val = Option.default (`DC 0.) source_val in
    solve_at s r source_val;
    (*r#dump_reals name 0.*)
    r#append_real name 0. (Solver.get_real s)
    
      
  type sweep_spec = { name    : string;  (* name of the sweep *)
		      id      : int;     (* ID of source to be swept *)
		      start   : float;   (* starting value *)
		      stop    : float;   (* final value *)
		      steps   : int }    (* # of steps *)

  let create_sweep name id start stop steps =
    { name=name; id=id; start=start; stop=stop; steps=steps}
      
  let sweep s r spec =
    let rec sweep_aux cur_step =
      if cur_step < spec.steps then
	let cur_val = spec.start +. (float cur_step) *. (
	  (spec.stop -. spec.start) /. (float spec.steps)) in
	info "Solving DC sweep at %g" cur_val;
	solve_at s r (`DC_SWEEP (spec.id, cur_val));
	r#append_real spec.name cur_val (Solver.get_real s);
	sweep_aux (cur_step + 1)
    in
    sweep_aux 0
	
      
end;;

module AcAnalysis = functor (Solver: SOLVER) -> struct
  (*
    An AC analysis tests the response of the linearized circuit to
    a single-tone frequency of a particular magnitude.  The tone is
    assumed to be "small"... that is, there is no error-checking but
    the analysis assumes that the linearized model is valid over the
    entire range of the input amplitude.

    NOTES:
     - currently *does not* support non-linear elements (see HB analysis)
     - all AC sources in the circuit are set to the same frequency
  *)
  
  type t = { name : string;     (* general analysis name *)
             range_type : range_type;
             start   : float;   (* starting frequency *)
             stop    : float;   (* final frequency *)
             steps   : int }    (* steps per decade || # of steps *)
  and range_type = LogRange | LinRange

  let create_log name start stop steps =
    { name=name; range_type=LogRange; start=start; stop=stop; steps=steps}

  let create_linear name start stop steps =
    { name=name; range_type=LinRange; start=start; stop=stop; steps=steps}

  let solve_at ?name:(name="ac") (s:Solver.t) (r:recorder) freq =
    (*if true then failwith "this is not right directly below";*)
    Solver.set_b_vec s `AC_MAG;
    Solver.solve s freq;
    r#append_complex name freq (Solver.get_complex s)

  let run t (s:Solver.t) (r:recorder) =
    match t.range_type with
        LogRange ->
          let interval = 1.0 /. (float t.steps) in
          let num_points = int_of_float
            (((log10 t.stop) -. (log10 t.start)) /. interval) in
            for point = 0 to (num_points-1) do
              let freq = 10. ** ((log10 t.start) +.
                                    ((float point) *. interval)) in
                solve_at ~name:t.name s r freq
            done
      | LinRange ->
          failwith "not implemented"

end;;



module Engine = functor (Solver: SOLVER) -> struct
  open ShootingMethod
    
  module Solver = Solver
  module Builder = Builder.Builder(Solver)
  module AC = AcAnalysis(Solver)
  module DC = DcAnalysis(Solver)
  module Tran = TranEngine.TranAnalysis(Solver)
  module HB = HarmonicBalance.HB(Solver)
  module ShootingMethod = ShootingMethod(Solver)
  module Volterra = Volterra.Volterra(Solver)
end;;

module GslEngine = Engine(Gsl_solver.GslSolver);;
