open OUnit
open Engine
open Printf
open Util.TestHelpers
open Gsl_solver

let f1 a b ~x =
  let x0 = x.{0} in
    a *. (1. -. x0)

let df1 a b ~x ~y =
  y.{0} <- ~-. a ;
  y.{1} <- 0.

let f2 a b ~x =
  let x0 = x.{0} in
  let x1 = x.{1} in
    b *. (x1 -. x0 *. x0)

let df2 a b ~x ~y =
  let x0 = x.{0} in
    y.{0} <- -2. *. b *. x0 ;
    y.{1} <- b

let fcmp a b =
  assert_equal ~cmp:(OUnit.cmp_float ~epsilon:0.1)
    ~printer:string_of_float a b
      
let suite = "gsl" >:::
  [
    "octave_example" >::
      (* function y = f (x)
	 y(1) = -2*x(1)^2 + 3*x(1)*x(2)   + 4*sin(x(2)) - 6;
	 y(2) =  3*x(1)^2 - 2*x(1)*x(2)^2 + 3*cos(x(1)) + 4;
	 endfunction *)
      (fun () ->
	let s = GslSolver.create 2 in
	GslSolver.add_nonlinear_eq s 0 ~f:(fun ~x -> 
	  -2. *. (x.{0} ** 2.) +. 3. *. x.{0} *. x.{1} +.
	    4. *. (sin x.{1})) ();
	GslSolver.add_nonlinear_eq s 1 ~f:(fun ~x ->
	  3. *. (x.{0} ** 2.) -. 2. *. x.{0} *. (x.{1} ** 2.) +.
	    3. *. (cos x.{0})) ();
	GslSolver.set_source_vec s 0 [(fun _ -> 6.)];
	GslSolver.set_source_vec s 1 [(fun _ -> -.4.)];
	GslSolver.set_b_vec s (`DC 0.);
	let init_x = [|1.1; 2.|] in
	let status = GslSolver.solve_nonlinear_noderiv ~init_x s in
	"converged" @? (status = Solver.Converged);
	fcmp 0.57983 (GslSolver.get_real s 0);
	fcmp 2.54621 (GslSolver.get_real s 1);
      );
    (*"simple_cicuit_noderiv" >::
      (fun () ->
	let square i ~x = (x.{1} -. x.{2}) ** 2.
	and neg_square i ~x = -.1. *. ((x.{1} -. x.{2}) ** 2.) in
	let s = GslSolver.create 3 in
	let a = GslSolver.set_a_matrix s in
	a 0 0 1.; a 1 1 1.; a 0 1 (-.1.); a 1 0 (-.1.);
	a 1 1 1.; a 2 2 1.;
	GslSolver.set_source_vec s 0 (fun _ -> 1.);
	GslSolver.add_nonlinear_eq s 1 ~f:(square 1) ();
	GslSolver.add_nonlinear_eq s 2 ~f:(neg_square 2) ();
	GslSolver.set_b_vec s (`DC 0.);
	let status = GslSolver.solve_nonlinear_noderiv s in
	"converged" @? (status = Solver.Converged);
      );*)
    "simple_diode_setup_noderiv" >::
      (* taken from pg 431 of Computer methods for circuit analysis & design *)
      (fun () ->
	let diode_f i ~x = (exp (40. *. x.{i})) -. 1. in
	let s = GslSolver.create 2 in
	  GslSolver.set_a_matrix s 0 0 1.;
	  GslSolver.set_a_matrix s 1 1 1.;
	  GslSolver.set_a_matrix s 0 1 (-.1.);
	  GslSolver.set_a_matrix s 1 0 (-.1.);
	  GslSolver.set_source_vec s 0 [(fun _ -> 1.)];
	  GslSolver.add_nonlinear_eq s 0 ~f:(diode_f 0) ();
	  GslSolver.add_nonlinear_eq s 1 ~f:(diode_f 1) ();
	  GslSolver.set_b_vec s (`DC 0.);
	  let status = GslSolver.solve_nonlinear_noderiv s in
	  "converged" @? (status = Solver.Converged);
	  fcmp 0.01712 (GslSolver.get_real s 0);
	  fcmp 0.00041 (GslSolver.get_real s 1);
      );

    "simple_diode_setup" >::
      (* taken from pg 431 of Computer methods for circuit analysis & design *)
      (fun () ->
	let diode_f i ~x = (exp (40. *. x.{i})) -. 1. in
	let diode_df i ~x ~y =
	  y.{i} <- (40. *. (exp (40. *. x.{i}))) in
	let s = GslSolver.create 2 in
	  GslSolver.set_a_matrix s 0 0 1.;
	  GslSolver.set_a_matrix s 1 1 1.;
	  GslSolver.set_a_matrix s 0 1 (-.1.);
	  GslSolver.set_a_matrix s 1 0 (-.1.);
	  GslSolver.set_source_vec s 0 [(fun _ -> 1.)];
	  GslSolver.add_nonlinear_eq s 0 ~f:(diode_f 0) ~df:(diode_df 0) ();
	  GslSolver.add_nonlinear_eq s 1 ~f:(diode_f 1) ~df:(diode_df 1) ();
	  GslSolver.set_b_vec s (`DC 0.);
	  let status = GslSolver.solve_nonlinear s in
	  "converged" @? (status = Solver.Converged);
	  let v1, v2 = (GslSolver.get_real s 0), (GslSolver.get_real s 1) in
	  printf "%g,%g\n%!" v1 v2;
	  fcmp 0.01712 v1;
	  fcmp 0.00041 v2;
      );

    "non_linear_equations" >::
      (fun () ->
        let s = GslSolver.create 2 in
          GslSolver.add_nonlinear_eq s 0 ~f:(f1 1. 10.) ~df:(df1 1. 10.) ();
          GslSolver.add_nonlinear_eq s 1 ~f:(f2 1. 10.) ~df:(df2 1. 10.) ();
          let status = GslSolver.solve_nonlinear s in
            "converged" @? (status = Solver.Converged);
            1. @=.? (GslSolver.get_real s 0);
            1. @=.? (GslSolver.get_real s 1);
      );

   "non_linear_equations_noderiv" >::
     (fun () ->
       let s = GslSolver.create 2 in
          GslSolver.add_nonlinear_eq s 0 ~f:(f1 1. 10.) ();
          GslSolver.add_nonlinear_eq s 1 ~f:(f2 1. 10.) ();
          let status = GslSolver.solve_nonlinear_noderiv s in
            "converged" @? (status = Solver.Converged);
            1. @=.? (GslSolver.get_real s 0);
            1. @=.? (GslSolver.get_real s 1);
     );

   "vector subtraction" >::
     (* NOTE: the results go into the first argument: i.e
	Gsl_vector sub v1 v2 <--> v1 <- v1 - v2 *)
     (fun () ->
       let v1 = Gsl_vector.of_array [|1.; 2.; 3.|] in
       let v2 = Gsl_vector.of_array [|3.; 3.; 3.;|] in
         Gsl_vector.sub v1 v2;
         -.2. @=.? v1.{0};
         -.1. @=.? v1.{1};
         0. @=.? v1.{2}
     );

   "vector addition" >::
     (* NOTE: the results go into the first argument: i.e
	Gsl_vector add v1 v2 <--> v1 <- v1 + v2 *)
     (fun () ->
       let v1 = Gsl_vector.of_array [|1.; 2.; 3.|] in
       let v2 = Gsl_vector.of_array [|3.; 3.; 3.;|] in
         Gsl_vector.add v1 v2;
         4. @=.? v1.{0};
         5. @=.? v1.{1};
         6. @=.? v1.{2}
     );

   (*"slice left" >::
     (fun () ->
       let m1 = Gsl_matrix.of_arrays [|
	 [|1.; 1.; 1.;1.|]; [|2.; 2.; 2.;2.|]; [|3.;3.;3.;3.|];
	 [|4.;4.;4.;4.|]|] in
       let m2 = Bigarray.genarray_of_array2 m1 in
       let m3 = Bigarray.Genarray.slice_left m2 [|1;|] in
       let m4 = Bigarray.array2_of_genarray m3 in
       let rows, cols = Bigarray.Array2.dim1 m4, Bigarray.Array2.dim2 m4 in
       2 @=? rows;
       2 @=? cols;
     );*)

   "matrix * vector" >::
     (fun () ->
       let v1 = Gsl_vector.of_array [|1.; 2.; 3.;|] in
       let result = Gsl_vector.create 3 in
       let m1 = Gsl_matrix.of_arrays [|
	 [|1.; 1.; 1.;|]; [|2.; 2.; 2.;|]; [|3.;3.;3.;|] |] in
       Gsl_blas.gemv Gsl_blas.NoTrans ~alpha:1. ~a:m1 ~x:v1 ~beta:1.
	 ~y:result;
       6. @=.? result.{0};
       12. @=.? result.{1};
       18. @=.? result.{2};
     );
       

  ];;

let _ =
  run_test_tt_main suite
