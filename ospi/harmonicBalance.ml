open Printf
open Solver
open Recorder
open Complex
open Gsl_helpers
open ExtList
open Util.ListHelpers

let debug = Circuit.debug
let info = Circuit.info
let warn = Circuit.warn

module HB = functor (Solver: SOLVER) -> struct
  type t = {
    circuit_size : int;      (* Number of variables in the MNA *)
    sample_size  : int;      (* Total number of time domain samples *)
    harmonic_map : (int*float) list;  (* Map of harmonics in solution *)
    total_size   : int;      (* Total size of the HB matrix *)
    permut_m     : M.matrix; (* Permutation matrix *)
    permut_m'    : M.matrix; (* Inverse/transpose permutation matrix *)
    y_bar        : M.matrix; (* Static HB matrix *)
    dft_m        : M.matrix; (* Block diagonal DFT matrix *)
    idft_m       : M.matrix; (* Block diagonal inverse DFT matrix *)
    b_bar        : V.vector; (* Source vector *)
    x_bar        : V.vector; (* Results vector *)
    x_init       : V.vector; (* DC solution to start with *)
  }

  let print t =
    fhtml_of_matrix ~file:"hb_engine.xml"
      [("y_bar",(`M t.y_bar)); ("permut", (`M t.permut_m));
       ("b_bar",(`V t.b_bar)); ("x_bar", (`V t.x_bar));
       ("idft",(`M t.idft_m)); ("dft", (`M t.dft_m));]
    
  let harmonic_to_node t v_h =
    let v_n = V.create ~init:0. t.total_size in
    for i = 0 to t.total_size - 1 do
      let index = (i * t.circuit_size) in
      let index = (index / t.total_size) + (index mod t.total_size) in
      v_n.{i} <- v_h.{index}
    done;
    v_n

  let node_to_harmonic t v_n =
    let v_h = V.create ~init:0. t.total_size in
    for i = 0 to t.total_size - 1 do
      let index = i * t.sample_size in
      let index = (index / t.total_size) + (index mod t.total_size) in
      v_h.{i} <- v_n.{index}
    done;
    v_h

  let apply_gamma_gamma' t dfdx =
    (* Now, we have to apply gamma and gamma' to the sub matrices *)
    let count = t.sample_size in
    let gamma' = Dft.gamma' count in
    let gamma = Dft.gamma count in
    let temp_m1 = M.create count count in
    let temp_m2 = M.create count count in
    let dfdx_bar_node = M.create t.total_size t.total_size in
    for i = 0 to t.circuit_size - 1 do
      for j = 0 to t.circuit_size - 1 do
	let offset_i = i * count in
	let offset_j = j * count in
	for k = 0 to count - 1 do
	  for l = 0 to count - 1 do
	    temp_m1.{k,l} <- dfdx.{offset_i+k, offset_j+l}
	  done
	done;
	(gamma |*| temp_m1) temp_m2;
	(temp_m2 |*| gamma') temp_m1;
	for k = 0 to count - 1 do
	  for l = 0 to count - 1 do
	    dfdx_bar_node.{offset_i+k, offset_j+l} <- temp_m1.{k,l}
	  done
	done;
      done;
    done;
    dfdx_bar_node

  let max_multiroot_steps = 500     (* 200 *)
  let multiroot_epsabs = 1e-12      (* 1e-9 *)
  let source_stepping_steps = 10000 (* 100 *)
  let multiroot_solver = Gsl_multiroot.Deriv.HYBRIDSJ  (* HYBRIDSJ *)
      
  let solve_nonlinear solver t =
    let maxiter = max_multiroot_steps
    and epsabs = multiroot_epsabs 
    and solver_method = multiroot_solver in  (* HYBRIDSJ *)
    (* Create some intermediate vectors/matrices *)
    let x_node = V.create ~init:0. t.total_size
    and f_node = V.create ~init:0. t.total_size
    and f_bar_node = V.create ~init:0. t.total_size in

    let dfdx = M.create ~init:0. t.total_size t.total_size in
    let temp_m = M.create ~init:0. t.circuit_size t.circuit_size in
    let temp2_m = M.create t.total_size t.total_size
    and dfdx_bar = M.create t.total_size t.total_size in
    
    let f_count = ref 0
    and df_count = ref 0 and f_time = ref 0. and df_time = ref 0. in
    let f ~x:x_bar ~f:y =
      incr f_count;
      let t1 = Unix.gettimeofday () in
      (* Y*X + F(X) - B *)
      (* First the hard part, computation of F(X) *)
      (* Step 1: convert last guess to node major ordering *)
      let x_bar_node = harmonic_to_node t x_bar in
      (* Step 2: perform IFFT on each node --> time domain waveforms *)
      (t.idft_m |*> x_bar_node) x_node;
      (* Step 3: compute the nonlinear components *)
      let x_node' = node_to_harmonic t x_node in (* groups things correctly *)
      for i = 0 to t.sample_size - 1 do (* for each time point *) 
	let offset = i * t.circuit_size in
	let x_node'_sub = V.subvector x_node' ~off:offset ~len:t.circuit_size
	and f_node_sub = V.subvector f_node ~off:offset ~len:t.circuit_size in
	Solver.apply_nonlinear_functions solver ~x:x_node'_sub ~y:f_node_sub;
      done;
      let f_node = harmonic_to_node t f_node in (* ungroup things back *)
      (* Step 4: perform FFT on each node *)
      (t.dft_m |*> f_node) f_bar_node;
      (* Step 5: reorder to harmonic-major form *)
      let f_bar = node_to_harmonic t f_bar_node in
      (* Final Computations *)
      (t.y_bar |*> x_bar) y;  (* y <- Y * X *)
      V.add y f_bar;          (* Y <- Y + F *)
      V.sub y t.b_bar;        (* Y <- Y - B *)
      f_time := !f_time +. ((Unix.gettimeofday ()) -. t1)

    and df ~x:x_bar ~j =
      incr df_count;
      let t1 = Unix.gettimeofday () in
      let count = t.sample_size in
      (* Y + dF(x)/dX *)
      (* Step 1: convert last guess to node major ordering *)
      let x_bar_node = harmonic_to_node t x_bar in
      (* Step 2: perform IFFT on each node --> time domain samples *)
      (t.idft_m |*> x_bar_node) x_node;
      (* Step 3: compute the partial derivatives *)
      let x_node' = node_to_harmonic t x_node in (* groups things correctly *)
      for i = 0 to count - 1 do (* for each sample/time point *)
	M.set_zero temp_m;
	let offset = i * t.circuit_size in
	(* x_node'_sub is the vector of circuit values at time ti *)
	let x_node'_sub = V.subvector x_node' ~off:offset ~len:t.circuit_size in
	Solver.apply_nonlinear_derivatives solver ~x:x_node'_sub ~j:temp_m;
	for j = 0 to t.circuit_size - 1 do
	  for k = 0 to t.circuit_size - 1 do
	    let row = j * count + i
	    and col = k * count + i in
	    dfdx.{row,col} <- temp_m.{j,k}
	  done
	done;
      done;
      let dfdx_bar_node = apply_gamma_gamma' t dfdx in
      (t.permut_m' |*| dfdx_bar_node) temp2_m;
      (temp2_m |*| t.permut_m) dfdx_bar;
      M.memcpy ~src:t.y_bar ~dst:j;
      M.add j dfdx_bar;
      df_time := !df_time +. ((Unix.gettimeofday ()) -. t1)
    in
    let fdf ~x ~f:y ~j =
      f ~x ~f:y;
      df ~x ~j in
    let gf = {
      Gsl_fun.multi_f = f;
      Gsl_fun.multi_df = df;
      Gsl_fun.multi_fdf = fdf; }
    in
    let solv = Gsl_multiroot.Deriv.make solver_method t.total_size gf t.x_bar in
    let rec proc iter =
      Gsl_multiroot.Deriv.iterate solv;
      let status = Gsl_multiroot.Deriv.test_residual solv epsabs in
      match status with
        | true -> Converged, iter
        | false when iter >= maxiter -> IterationLimitExceeded, iter
        | false -> proc (succ iter)
    in
    let status, iters = proc 1 in
    Gsl_multiroot.Deriv.get_state solv ~x:t.x_bar ();
    info "Solver Stats: iters=%d f=%d (%g s) df=%d (%g s)"
      iters !f_count !f_time !df_count !df_time;
    status

  let set_source_vector freq_mapping solver ~b_bar ~alpha =
    let circuitSize = Solver.size solver in
    (* First, lets retrieve the DC components *)
    let b_vec = Solver.get_b_vec solver (`DC 0.) in
    for i = 0 to (V.length b_vec) - 1 do
      b_bar.{i} <- b_vec.{i};
    done;
    (* Now, go through the remaining frequencies *)
    List.iter
      (fun (i, f) ->
	 (* Second, the REAL (cosine) terms of the AC sources *)
	 let b_vec_real = (Solver.get_b_vec solver (`AC_REAL f)) in
	 for j = 0 to (V.length b_vec) - 1 do
	   b_bar.{j + (2*i-1)*circuitSize} <- alpha *. b_vec_real.{j};
	 done;
	 (* Finally, the IMAG (sine) terms of the AC sources *)
	 let b_vec_imag = (Solver.get_b_vec solver (`AC_IMAG f)) in
	 for j = 0 to (V.length b_vec) - 1 do
	   b_bar.{j + (2*i)*circuitSize} <- alpha *. b_vec_imag.{j};
	 done
      ) freq_mapping
      
  let create solver ~xinit ~harmonic_map =
    let xinit = V.of_array xinit in
    let circuitSize = Solver.size solver in
    let sample_size = 2 * (List.length harmonic_map) + 1 in
    let total_size = circuitSize * sample_size in
    info "HB Problem Size: MNA=%d harmonics=%d size=%d" circuitSize
      sample_size total_size;
    (* Construct the permutation and permutation transform matrices *)
    let permut = M.create ~init:0. total_size total_size in
    let permut' = M.create ~init:0. total_size total_size in
    for i = 0 to total_size - 1 do
      let index = (i * circuitSize) in
      let index = (index / total_size) + (index mod total_size) in
      permut.{i,index} <- 1.;
    done;
    M.transpose permut' permut;
    (* Ask for the y_bar matrix to be calculated *)
    let y_bar = Solver.get_y_bar_matrix2 solver
      ~harmonics:(List.length harmonic_map)
      ~ws:(List.map (fun (_,w) -> w) harmonic_map) in
    (* Create the source b_bar vector *)
    let b_bar = V.create ~init:0. total_size in
    set_source_vector harmonic_map solver ~b_bar ~alpha:1.;
    (* Create the x_bar initial vector *)
    let x_bar = V.create ~init:0. total_size in
    for i = 0 to (V.length xinit) - 1 do
      x_bar.{i} <- xinit.{i}
    done;
    (* Create the IDFTT, and DFT matrix *)
    let idft_m = M.create ~init:0. total_size total_size 
    and dft_m = M.create ~init:0. total_size total_size
    and gamma' = Dft.gamma' sample_size
    and gamma = Dft.gamma sample_size in
    for i = 0 to circuitSize - 1 do
      let shift = i * sample_size in
      blit gamma' shift shift idft_m;
      blit gamma shift shift dft_m;
    done;
    let t = { circuit_size   = circuitSize;
	      sample_size    = sample_size;
	      harmonic_map   = harmonic_map;
	      total_size     = total_size;
	      permut_m       = permut;
	      permut_m'      = permut';
	      idft_m         = idft_m;
	      dft_m          = dft_m;
	      y_bar          = y_bar;
	      b_bar          = b_bar;
	      x_bar          = x_bar;
	      x_init         = V.copy xinit;
    } in
    (*Solver.print solver;*)
    (*print t;*)
    t

  (* Creates a single-tone Harmonic Balance solver with "size" harmonics
     starting from a base frequency of w: i.e. w, 2*w, ... size*w *)
  let create_single_tone solver ~xinit ~size ~w =
    let harmonic_map = List.init size
      (fun i -> ((i+1), (w *. (float (i+1))))) in
    create solver ~xinit ~harmonic_map

  let create_two_tone solver ~xinit ~k1 ~w1 ~k2 ~w2 =
    let forward, reverse = Assign3.frequency_mapping [k1;k2] [w1;w2] in
    let forward = List.sort ~cmp:(fun (p1,_) (p2,_) -> compare p1 p2) forward in
    let forward = List.filter (fun (p,_) -> p <> 0) forward in
    let harmonic_map = List.map (fun (p,(_,f)) -> (p,f)) forward in
    create solver ~xinit ~harmonic_map

  (*List.iter (fun (p,(ks,f)) ->
    printf "p=%d --> " p;
    list_iteri (fun i k -> printf "k(%d)=%d " (i+1) k) ks;
    printf " f=%g\n%!" f) forward;
    List.iter (fun (f, p) -> printf "f=%g --> %d\n" f p) reverse;*)

  let dump_results t =
    for i = 0 to t.sample_size - 1 do
      printf "-- %d th Harmonic -- \n%!" i;
      for j = 0 to t.circuit_size - 1 do
	let index = i * t.circuit_size + j in
	printf "xbar(%d) = %g\n" j t.x_bar.{index}
      done
    done

  let ratio_threshold = 1e-6
  exception ConvergenceFailed of string

  let solve_with_stepping t solver =
    info "HB: starting source stepping";
    let num_steps = source_stepping_steps in
    let start_time = Unix.gettimeofday () in
    (* Set the x_bar initial vector *)
    V.set_zero t.x_bar;
    for i = 0 to (V.length t.x_init) - 1 do
      t.x_bar.{i} <- t.x_init.{i}
    done;
    for i = 0 to num_steps do
      printf ".%!";
      let alpha = (float i) /. (float num_steps) in
      set_source_vector t.harmonic_map solver ~b_bar:t.b_bar ~alpha;
      printf " alpha=%g " alpha;
      let result = solve_nonlinear solver t in
      match result with
	| Converged -> ()
	| IterationLimitExceeded ->
	    let s = sprintf "HB source stepping failed at alpha=%g\n%!" alpha in
	    raise (ConvergenceFailed s)
    done;
    printf "\n";
    let duration = (Unix.gettimeofday ()) -. start_time in
    info "HB: source stepping finished (steps=%d time=%g)" num_steps duration

  let store_results ~name t (r:recorder) =
    (* Check the answer
       - for each node, compare the magnitude of the highest
       component to the fundamental and see if exceeds our threshold *)
    (*let ratios = V.create t.circuit_size in
    let max_freq = Array.fold_left max neg_infinity t.harmonics in
    let max_freqi = Array.findi (fun x -> x=max_freq) t.harmonics in*)
    (*for i = 0 to t.circuit_size - 1 do
      let w0_real = t.x_bar.{ t.circuit_size + i }
      and w0_imag = t.x_bar.{ (t.circuit_size * 2) + i }
      and wn_real = t.x_bar.{ (t.circuit_size * (t.harmonics*2 - 1)) + i }
      and wn_imag = t.x_bar.{ (t.circuit_size * (t.harmonics*2)) + i }
      in
      let ratio = (Complex.norm {re=wn_real; im=wn_imag}) /.
	(Complex.norm {re=w0_real; im=w0_imag}) in
      ratios.{i} <- match classify_float ratio with
	| FP_normal -> ratio
	| FP_nan -> 0.
	| FP_infinite -> warn "Got an infinite ratio!!!"; 0.
	| _ -> 0.
    done;
    (*let max_ratio = V.max ratios in*)
    let max_ratio = ratios.{ (V.length ratios) - 1} in
    if max_ratio > ratio_threshold then begin
      warn "Ratio of w[n] to w[1] exceeds threshold: %g > %g"
	max_ratio ratio_threshold;
      warn "You should run the simulation with more harmonics!!!"
    end;
    info "Max Ratio: %g" (V.max ratios);*)
    (* record the answer *)
    r#append_complex name 0. (fun j -> {re=t.x_bar.{j}; im=0.});
    for i = 0 to (List.length t.harmonic_map) - 1 do
      r#append_complex name (List.assoc (i+1) t.harmonic_map)
	(fun j ->
	  {re=t.x_bar.{(2*i+1)*t.circuit_size + j};
	   im=t.x_bar.{(2*i+2)*t.circuit_size + j}})
    done
      
  let solve ~name t solver r =
    info "Performing HB analysis (%s)" name;
    let start_time = Unix.gettimeofday () in
    let result = solve_nonlinear solver t in
    let duration = (Unix.gettimeofday ()) -. start_time in
    match result with
      | Converged ->
	  info "HB analysis complete (WALL time=%g s). Storing results." duration;
	  store_results ~name t r
      | IterationLimitExceeded ->
	  warn "HB analysis: failed to converge (WALL time=%g s)" duration;
	  info "Starting source stepping algorithm";
	  solve_with_stepping t solver;
	  let total_duration = (Unix.gettimeofday ()) -. start_time in
	  info "HB analysis: PROBLEM_SIZE=%d TOTAL WALL TIME=%g s"
	    t.total_size total_duration;
	  store_results ~name t r

  let solve_linear ~name t r =
    let mna_matrix = Gsl_matrix.copy t.y_bar in
    let answer = Gsl_linalg.solve_LU (`M mna_matrix) (`V t.b_bar) in
    V.memcpy ~src:(V.of_array answer) ~dst:t.x_bar;
    warn "results are not being stored";
    dump_results t
  
end;;
