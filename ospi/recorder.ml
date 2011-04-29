open Printf
open ExtHashtbl
open ExtList
open ExtString
open Solver
open Circuit
open Gsl_helpers

type analysis_map = (string, signal_set) Hashtbl.t
and signal_set = sweep * ((id, trace) Hashtbl.t)
and id = int
and signal_map = (string, signal) Hashtbl.t
and sweep = float DynArray.t
and sweep_type = [`Complex | `Real]
and trace = [
  `Complex of Complex.t DynArray.t
| `Real of float DynArray.t]
and signal = [
  `Complex of Value.complex_signal
| `Real of Value.real_signal]

let analysis_names data =
  Enum.fold (fun name total -> total ^ ", " ^ name) "" (Hashtbl.keys data)
    
let find_trace data analysis id =
  let sweep, signal_set =
    try
      Hashtbl.find data analysis
    with Not_found ->
      failwith (sprintf "%s not found in %s\n%!" analysis (analysis_names data))
  in
  try
    Hashtbl.find signal_set id
  with Not_found ->
    failwith (sprintf "signal with id=%d not found\n%!" id)
  

let find_sweep data analysis =
  try
    let sweep, _ = Hashtbl.find data analysis in
    sweep
  with Not_found ->
    failwith (sprintf "%s not found in %s\n%!" analysis (analysis_names data))

exception CallingError
      
class recorder circuit = object (self)
  val data : analysis_map = Hashtbl.create 5
  val named_signals : signal_map = Hashtbl.create 1
  val circuit : circuit = circuit

  method init_analysis name =
    Hashtbl.replace data name (DynArray.create (), (Hashtbl.create 5))

  (* Retrieves the result for node_id at the sweep point sweep_val
     or raise Not_found if the sweep points doesn't exist. *)
  method val_at analysis node_id sweep_val =
    let trace = find_trace data analysis node_id
    and sweep = find_sweep data analysis in
    try
      let index = DynArray.index_of (fun x -> x = sweep_val) sweep in
      match trace with
	| `Complex x -> `Complex (DynArray.get x index)
	| `Real x -> `Real (DynArray.get x index)
    with Not_found ->
      (* Could try interpolation... slightly more trick for Complex *)
      failwith (sprintf "sweep value %g not found" sweep_val)

  method real_at analysis node_id sweep_val =
    match self#val_at analysis node_id sweep_val with
      | `Complex x -> x.Complex.re
      | `Real x -> x

  method dump_reals analysis sweep_val =
    let sweep, signal_set = Hashtbl.find data analysis in
    let index = DynArray.index_of (fun x -> x = sweep_val) sweep in
    Hashtbl.iter (fun id trace ->
      printf "%d: %g\n" id (match trace with
	| `Complex trace -> (DynArray.get trace index).Complex.re
	| `Real trace -> DynArray.get trace index)
    ) signal_set

  method imag_at analysis node_id sweep_val =
    match self#val_at analysis node_id sweep_val with
      | `Complex x -> x.Complex.im
      | `Real x -> x

  method mag_at analysis node_id sweep_val =
    match self#val_at analysis node_id sweep_val with
      | `Complex x -> Complex.norm x
      | `Real x -> abs_float x

  method phase_at analysis node_id sweep_val =
    match self#val_at analysis node_id sweep_val with
      | `Complex x -> Complex.arg x
      | `Real x -> 0.

  method html_of_signals ?file ?outc analysis node_ids =
    let outc = match file, outc with
      | Some f, None -> open_out f
      | None, Some c -> c
      | Some _, Some _ | None, None -> raise CallingError
    in
    let sweep = DynArray.to_list (find_sweep data analysis) in
    let sweep = List.sort ~cmp:compare sweep in
    fprintf outc "<table>\n";
    fprintf outc "<tr>";
    fprintf outc "<th>Sweep</th>";
    List.iter (fun id -> fprintf outc "<th>%d</th>" id) node_ids;
    fprintf outc "</tr>";
    List.iter
      (fun xval ->
	 fprintf outc "<tr><td>%g</td>" xval;
	 List.iter
	   (fun node_id ->
	      match self#val_at analysis node_id xval with
		| `Complex x ->
		    fprintf outc "<td>%g / %g</td>"
		      (Complex.norm x) (Complex.arg x)
		| `Real x ->
		    fprintf outc "<td>%g</td>" x
	   ) node_ids;
	 fprintf outc "</tr>\n";
      ) sweep;
    fprintf outc "</table>\n"
	  
  method get_signal analysis node_id : signal =
    let trace = find_trace data analysis node_id
    and sweep = find_sweep data analysis in
    match trace with
      | `Complex x ->
	  `Complex {Value.complex_sweep =
              (Value.farray_of_array (DynArray.to_array sweep));
		    Value.complex_trace =
              (Value.complex_array_of_array (DynArray.to_array x))}
      | `Real x ->
	  `Real {Value.real_sweep =
              (Value.farray_of_array (DynArray.to_array sweep));
		 Value.real_trace =
              (Value.farray_of_array (DynArray.to_array x))}
	    
  (* This routine makes a number of assumptions
     - the specified trace is complex and contains evenly spaced samples
  *)
  method get_ifft analysis node_id : signal =
    let trace = find_trace data analysis node_id
    and sweep = find_sweep data analysis in
    match trace with
      | `Complex trace ->
	  let n = 2 * (DynArray.length trace) - 1 in
	  let data = V.create n in
	  data.{0} <- (DynArray.get trace 0).Complex.re;
	  for i = 1 to (DynArray.length trace) - 1 do
	    data.{2*i-1} <- (DynArray.get trace i).Complex.re;
	    data.{2*i} <- (DynArray.get trace i).Complex.im;
	  done;
	  let result = Dft.idft data in
	  let sweep = Array.init n (fun i -> float i) in
	  `Real {Value.real_sweep = Value.farray_of_array sweep;
		 Value.real_trace = farray_of_vector result}

      (* FIXME: Figure out how to pack the array correctly for GSL IFFT *)
      (*let n = 2 * (DynArray.length trace) in
	let data = Array.make n 0. in
	for i = 0 to (DynArray.length trace) - 1 do
	data.(i*2) <- (DynArray.get trace i).Complex.re;
	data.(i*2 + 1) <- (DynArray.get trace i).Complex.im;
	done;
	let a = {Gsl_fft.layout = Gsl_fft.Halfcomplex;
	data = data } in
	let ws = Gsl_fft.Real.make_workspace n in
	let wt = Gsl_fft.Halfcomplex.make_wavetable n in
	Gsl_fft.Halfcomplex.inverse a wt ws;
	let sweep = Array.init n (fun i -> float i) in
	`Real {Value.real_sweep = Value.farray_of_array sweep;
	Value.real_trace = Value.farray_of_array data}*)
      | `Real x -> failwith "can only perform IFFT on complex signals"

  method get_named_signal name =
    Hashtbl.find named_signals name
      
  method load name file =
    let inf = open_in file
    and sweep = DynArray.create ()
    and trace = DynArray.create () in
    let rec read_line () =
      try
	let l = input_line inf in
	let pieces =  Array.of_list (String.nsplit l "\t") in
	if Array.length pieces = 2 then begin
	  let x,y = (float_of_string pieces.(0)),
	    (float_of_string pieces.(1)) in
	  DynArray.add sweep x;
	  DynArray.add trace y;
	  read_line ()
	end
      with
	| End_of_file -> ()
    in
    read_line ();
    close_in inf;
    Hashtbl.add named_signals name
      (`Real {Value.real_sweep = Value.farray_of_dynarray sweep;
	      Value.real_trace = Value.farray_of_dynarray trace})
    
	  
  method append_real analysis (x:float) (cb: int -> float) =
    if not (Hashtbl.exists data analysis) then
      self#init_analysis analysis;
    let sweep, signal_set = Hashtbl.find data analysis in
    DynArray.add sweep x;
    Hashtbl.iter (fun ext_id int_id ->
      if not (Hashtbl.exists signal_set ext_id) then
        Hashtbl.add signal_set ext_id (`Real (DynArray.create ()));
      let trace = Hashtbl.find signal_set ext_id
      and trace_val = cb int_id in
      match trace with 
	| `Complex _ -> failwith "mixing types"
	| `Real x -> DynArray.add x trace_val) circuit.node_map
      
  method append_complex analysis (x:float) (cb: int -> Complex.t) =
    if not (Hashtbl.exists data analysis) then
      self#init_analysis analysis;
    let sweep, signal_set = Hashtbl.find data analysis in
    DynArray.add sweep x;
    Hashtbl.iter (fun ext_id int_id ->
      if not (Hashtbl.exists signal_set ext_id) then
        Hashtbl.add signal_set ext_id (`Complex (DynArray.create ()));
      let trace = Hashtbl.find signal_set ext_id
      and trace_val = cb int_id in
      match trace with
	| `Real _ -> failwith "mixing types"
	| `Complex x -> DynArray.add x trace_val) circuit.node_map

end;;


