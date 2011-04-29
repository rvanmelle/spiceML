open Circuit
open Printf
open Solver
open Gsl_helpers
open Util.ListHelpers

let complex re im = { Gsl_complex.re = re; Gsl_complex.im = im }

(* Models sets of nonlinear equations of the form:
   a_matrix * x + c_matrix * dx/dt + f_vec = sources

   - equations may be solved in complex or real form depending
   on the analysis type
   - f_vec is the set of nonlinear equations and may be in a form
   either with or without derivatives but all must be consistent
*)
module GslSolver : SOLVER = struct

  type t = {
    size             : int;
    ac_matrix        : Gsl_matrix_complex.matrix;
    ar_matrix        : Gsl_matrix.matrix;
    cc_matrix        : Gsl_matrix_complex.matrix;
    cr_matrix        : Gsl_matrix.matrix;
    bc_vec           : Gsl_vector_complex.vector;
    br_vec           : Gsl_vector.vector;
    xc_vec           : Gsl_vector_complex.vector;
    xr_vec           : Gsl_vector.vector;
    f_vec            : ((f_type * df_type option) list) array;
    sources          : source_func list array;
  }

  let create size =
    let zero =  complex 0. 0. in {
      size      = size;
      ac_matrix = Gsl_matrix_complex.create ~init:zero size size;
      ar_matrix = Gsl_matrix.create ~init:0. size size;
      cc_matrix = Gsl_matrix_complex.create ~init:zero size size;
      cr_matrix = Gsl_matrix.create ~init:0. size size;
      bc_vec    = Gsl_vector_complex.create ~init:zero size;
      br_vec    = Gsl_vector.create ~init:0. size;
      xc_vec    = Gsl_vector_complex.create ~init:zero size;
      xr_vec    = Gsl_vector.create ~init:0. size;
      f_vec     = Array.make size [];
      sources   = Array.make size [(fun _ -> 0.)];
    }

  let get_y_bar_matrix t ~harmonics ~w =
    let total_size = t.size * (2 * harmonics + 1) in
    let y_m = Gsl_matrix.create ~init:0. total_size total_size in
    for i = 0 to 2 * harmonics do
      blit t.ar_matrix (i*t.size) (i*t.size) y_m;
    done;
    for i = 0 to harmonics - 1 do
      let w = w *. (float (i+1)) in
      let wo_c = Gsl_matrix.copy t.cr_matrix
      and wo_c' = Gsl_matrix.copy t.cr_matrix in
      Gsl_matrix.scale wo_c w;
      Gsl_matrix.scale wo_c' (-.w);
      let base = (i*2 + 1) * t.size in
      blit wo_c' (base + t.size) base y_m;
      blit wo_c base (base + t.size) y_m;
    done;
    y_m

  let get_y_bar_matrix2 t ~harmonics ~ws =
    if not ((List.length ws) = harmonics) then
      failwith (sprintf "inconsistent size: %d != %d" (List.length ws) harmonics);
    let total_size = t.size * (2 * harmonics + 1) in
    let y_m = Gsl_matrix.create ~init:0. total_size total_size in
    for i = 0 to 2 * harmonics do
      blit t.ar_matrix (i*t.size) (i*t.size) y_m;
    done;
    list_iteri (fun i w ->
		  let w = w *. (float (i+1)) in
		  let wo_c = Gsl_matrix.copy t.cr_matrix
		  and wo_c' = Gsl_matrix.copy t.cr_matrix in
		  Gsl_matrix.scale wo_c w;
		  Gsl_matrix.scale wo_c' (-.w);
		  let base = (i*2 + 1) * t.size in
		  blit wo_c' (base + t.size) base y_m;
		  blit wo_c base (base + t.size) y_m
	       ) ws;
    y_m
			      
  let set_a_matrix t i j value =
    Gsl_matrix_complex.set t.ac_matrix i j (complex value 0.);
    t.ar_matrix.{i,j} <- value

  let set_c_matrix t i j value =
    Gsl_matrix_complex.set t.cc_matrix i j (complex value 0.);
    t.cr_matrix.{i,j} <- value

  let set_b_vec ?(alpha=1.) t input_value =
    for i = 0 to (t.size-1) do
      let sources = t.sources.(i) in
      Gsl_vector_complex.set t.bc_vec i (complex 0. 0.);
      t.br_vec.{i} <- 0.;
      List.iter (fun source ->
		   let value = source input_value in
		   t.br_vec.{i} <- t.br_vec.{i} +. value;
		   Gsl_vector_complex.set t.bc_vec i
		     (Gsl_complex.add (Gsl_vector_complex.get t.bc_vec i)
			(complex value 0.))) sources
    done;
    Gsl_vector.scale t.br_vec alpha

  let get_b_vec ?alpha t input_value =
    set_b_vec ?alpha t input_value;
    t.br_vec

  let get_a_matrix t = t.ar_matrix
  let get_ac_matrix t = t.ac_matrix
  let get_c_matrix t = t.cr_matrix
  let get_cc_matrix t = t.cc_matrix

  let set_source_vec t i fn_list = t.sources.(i) <- fn_list

  let add_nonlinear_eq t i ~f ?df () =
    t.f_vec.(i) <- ((f, df) :: t.f_vec.(i))

  let get_complex t node_id =
    Gsl_vector_complex.get t.xc_vec node_id

  let get_real t node_id = t.xr_vec.{node_id}

  let size t = t.size

  let get_solution t = Gsl_vector.to_array t.xr_vec

  let solve t freq =
    let mna_matrix = Gsl_matrix_complex.copy t.cc_matrix in
      Gsl_matrix_complex.scale mna_matrix
        (complex 0. (2. *. Gsl_math.pi *. freq));
      Gsl_matrix_complex.add mna_matrix t.ac_matrix;

      let permut = Gsl_permut.create t.size in
        (* I have no idea what the call below returns *)
        ignore (Gsl_linalg.complex_LU_decomp (`CM mna_matrix) permut);
        Gsl_linalg.complex_LU_solve (`CM mna_matrix) permut (`CV t.bc_vec)
          (`CV t.xc_vec)

  let solve_nonlinear_noderiv ?(init_x) t =
    (* First, lets check the validity of this setup *)
    for i = 0 to (t.size - 1) do
      List.iter (fun pair ->
        match pair with
          | fn, None -> ()
	  | _ -> failwith "Inconsistent equation setup") t.f_vec.(i)
    done;
    (* things are good... let's begin *)
    let maxiter=100 and epsabs=1e-7
    and solver_method = Gsl_multiroot.NoDeriv.DNEWTON in
    let print_state_deriv n =
      let x = Gsl_vector.create n in
      let f = Gsl_vector.create n in
        fun iter solv ->
          Gsl_multiroot.NoDeriv.get_state solv ~x ~f () ;
          printf "iter = %3u x = %g %g f(x) = %g %g\n%!"
            iter x.{0} x.{1} f.{0} f.{1} ;
    in
    let f ~x ~f:y =
      for i = 0 to t.size - 1 do
	printf "x(%d) = %g\n" i x.{i};
      done;
      for i = 0 to (t.size - 1) do
	y.{i} <- 0.;
	List.iter (fun pair ->
          match pair with
            | fn, None ->
		let new_value = fn ~x in
		printf "new val (%d)=%g\n%!" i new_value;
		y.{i} <- (y.{i} +. new_value)
	    | _ -> failwith "Inconsistent equation setup") t.f_vec.(i)
      done;
      for i = 0 to t.size - 1 do
	printf "y'(%d) = %g\n" i y.{i};
      done;
      for i = 0 to t.size - 1 do
	printf "b(%d) = %g\n" i t.br_vec.{i};
      done;
      (* then, we need to substract the constant source vector *)
      Gsl_vector.sub y t.br_vec;
      (* finally, calculate the (a_matrix * x) and add it *)
      let temp_v = Gsl_vector.create ~init:0. t.size in
      Gsl_blas.gemv Gsl_blas.NoTrans ~alpha:1. ~a:t.ar_matrix
	~x ~beta:1. ~y:temp_v;
      for i = 0 to t.size - 1 do
	printf "temp(%d) = %g\n" i temp_v.{i};
      done;
      Gsl_vector.add y temp_v;
      for i = 0 to t.size - 1 do
	printf "y(%d) = %g\n" i y.{i};
      done;
    and x_init = match init_x with
      | Some x ->
	  if Array.length x != t.size then failwith "size mismatch";
	  Gsl_vector.of_array x;
      | None -> Gsl_vector.create ~init:0. t.size
    in
    let solv = Gsl_multiroot.NoDeriv.make solver_method t.size f x_init in
      printf "solver: %s\n" (Gsl_multiroot.NoDeriv.name solv) ;
      let rec proc iter =
        Gsl_multiroot.NoDeriv.iterate solv;
        print_state_deriv t.size iter solv;
        let status = Gsl_multiroot.NoDeriv.test_residual solv epsabs in
          match status with
            | true ->
                Printf.printf "status = converged (%d iterations)\n" iter;
                Converged
            | false when iter >= maxiter ->
                Printf.printf "status = too many iterations\n";
                IterationLimitExceeded
            | false ->
                proc (succ iter)
      in
      let status = proc 1 in
        Gsl_multiroot.NoDeriv.get_state solv ~x:t.xr_vec ();
        status

  (* FIXME: this is not right yet *)
  let apply_nonlinear_functions t ~x ~y =
    for i = 0 to (t.size - 1) do
      y.{i} <- 0.;  (* Fixme: should really be done by somebody else *)
      List.iter (fun pair ->
        match pair with
          | fn, Some _ -> y.{i} <- (y.{i} +. (fn ~x))
	  | _ -> failwith "Inconsistent equation setup") t.f_vec.(i)
    done

  let apply_nonlinear_derivatives t ~x ~j =
    for i = 0 to (t.size - 1) do
      let y = Gsl_matrix.row j i in
      List.iter (fun pair ->
        match pair with
          | _, Some fn -> fn ~x ~y;
	  | _ -> failwith "Inconsistent equation setup") t.f_vec.(i)
    done
	  
  let solve_nonlinear ?(init_x) t  =
    (* First, lets check the validity of this setup *)
    for i = 0 to (t.size - 1) do
      List.iter (fun pair ->
	match pair with
          | _, Some fn -> ()
	  | _ -> failwith "Inconsistent equation setup") t.f_vec.(i)
    done;
    (* Things are fine, let's begin *)
    let maxiter=1000 and epsabs=1e-7
    and solver_method = Gsl_multiroot.Deriv.NEWTON in
    let print_state_deriv n =
      let x = Gsl_vector.create n in
      let f = Gsl_vector.create n in
        fun iter solv ->
          Gsl_multiroot.Deriv.get_state solv ~x ~f () ;
          printf "iter = %3u x = %+ .3f %+ .3f f(x) = %+ .3e %+ .3e\n%!"
            iter x.{0} x.{1} f.{0} f.{1} ;
    in
    let f ~x ~f:y =
      (* Step1: compute the results from the nonlinear functions *)
      for i = 0 to (t.size - 1) do
	y.{i} <- 0.;
	List.iter (fun pair ->
          match pair with
            | fn, Some _ -> y.{i} <- (y.{i} +. (fn ~x))
	    | _ -> failwith "Inconsistent equation setup") t.f_vec.(i)
      done;
      (* then, we need to substract the constant source vector *)
      Gsl_vector.sub y t.br_vec;
      (* finally, compute (a_matrix * x) and add it to the result *)
      let temp_v = Gsl_vector.create ~init:0. t.size in
      Gsl_blas.gemv Gsl_blas.NoTrans ~alpha:1. ~a:t.ar_matrix
	~x ~beta:0. ~y:temp_v;
      Gsl_vector.add y temp_v;
    and df ~x ~j =
      Gsl_matrix.memcpy ~src:t.ar_matrix ~dst:j;
      for i = 0 to (t.size - 1) do
	let y = Gsl_matrix.row j i in
	List.iter (fun pair ->
          match pair with
            | _, Some fn -> fn ~x ~y;
	    | _ -> failwith "Inconsistent equation setup") t.f_vec.(i)
      done;
    in
    let fdf ~x ~f:y ~j =
      f ~x ~f:y;
      df ~x ~j
    in
    let gf = {
      Gsl_fun.multi_f = f;
      Gsl_fun.multi_df = df;
      Gsl_fun.multi_fdf = fdf; }
    and x_init = match init_x with
      | Some x ->
	  if Array.length x != t.size then failwith "size mismatch";
	  Gsl_vector.of_array x;
      | None -> Gsl_vector.create ~init:0. t.size
    in
    let solv = Gsl_multiroot.Deriv.make solver_method t.size gf x_init in
      (*printf "solver: %s\n" (Gsl_multiroot.Deriv.name solv) ;*)
      let rec proc iter =
        Gsl_multiroot.Deriv.iterate solv;
        (*print_state_deriv t.size iter solv;*)
        let status = Gsl_multiroot.Deriv.test_residual solv epsabs in
          match status with
            | true ->
                (*Printf.printf "status = converged\n";*)
                Converged
            | false when iter >= maxiter ->
                (*Printf.printf "status = too many iterations\n";*)
                IterationLimitExceeded
            | false ->
                proc (succ iter)
      in
      let status = proc 1 in
        Gsl_multiroot.Deriv.get_state solv ~x:t.xr_vec ();
        status

  let print ?(file="gsl_solver.xml") t =
    fhtml_of_matrix ~file
      [("ar_matrix",(`M t.ar_matrix));
       ("cr_matrix",(`M t.cr_matrix));
       ("br_vec", (`V t.br_vec));]
	  
  let print_solution t c =
    Hashtbl.iter (fun ext int ->
      let x = Gsl_vector_complex.get t.xc_vec int in
        printf "%d,%d = %g %g\n%!" int ext x.Gsl_complex.re x.Gsl_complex.im)
      c.node_map

end;;



