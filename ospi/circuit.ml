(**
   This module provides the data structures and building routines for the
   representation of circuits using modified nodal analysis.

   Functions are provided to stamp standard elements into the circuit matrix.
   An intermediate and more convenient representation is used while the
   circuit matrix is being built.  The final matrix is obtained afterward.

*)
open ExtHashtbl
open Printf
module PP = PrettyPrintHelpers
  
let debug_mode = ref false
let enable_debug () = debug_mode := true
let disable_debug () = debug_mode := false
let debug s = if !debug_mode then printf "%s%!" s
let debug fmt = if !debug_mode then Printf.fprintf fmt else Printf.ifprintf fmt

let debug fmt =
  if !debug_mode then PP.debug "ospi" fmt else PP.nodebug "ospi" fmt
let info fmt = Printf.kfprintf (fun ch -> Printf.printf "\n%!") stdout fmt
let warn fmt =
  Printf.printf "WARNING: ";
  Printf.kfprintf (fun ch -> Printf.printf "\n%!") stdout fmt

(* Basic circuit type declarations *)   
type node_data_location = node_id * node_id
and node_data_value = float
and node_id = int
and internal_node_id = [ `Ground | `Node of node_id ]
and time = float and frequency = float
and magnitude = float and phase = float
and node_data_map = (node_data_location, float) Hashtbl.t
and signal_type = Voltage | Current
and source_type = [`AC | `DC]
and circuit_node = [`Ground | `Node of int]
and source = [
  `AC of magnitude * phase
| `DC of (time -> float) ]
and admittance = [`G of float | `Y of float]
and impedance = [`R of float | `Z of float]
and source_func = source_func_input -> float
and source_func_input = [
| `AC_REAL of float
| `AC_IMAG of float
| `AC_MAG
| `DC_SWEEP of node_id * float
| `DC of float
| `DC_DERIV of float ]
and sign = [`NEG | `POS]
and unsigned_df_type = sign -> df_type
and f_type = x:Gsl_vector.vector -> float
and df_type = x:Gsl_vector.vector -> y:output_vector -> unit
and output_vector = Gsl_vector.vector
and source_map_type = [
  `VSRC of node_id
| `ISRC of internal_node_id * internal_node_id]
and source_id = int
and circuit = {
  node_map             : (int, int) Hashtbl.t;
  source_map           : (source_id, source_map_type) Hashtbl.t;
  g_map                : node_data_map;
  c_map                : node_data_map;
  x_array              : signal_type DynArray.t;
  b_map                : (int, source_func list) Hashtbl.t;
  f_map                : (int, ((f_type * df_type option) list)) Hashtbl.t;
  mutable subckt_index : int;
  mutable subckt_p     : bool;
}

module Source = struct
  module AC = struct
    type source_type = SIN | COS
    type source = {
      source_type : source_type;
      mag         : float;   (* magnitude in volts *)
      freq        : float;   (* frequency in rads/s *)
      phase       : float;   (* phase offset in rads *)
    }
	
    let vac ?(src_type=COS) ?(phase=0.) ~freq ~mag () =
      { source_type = src_type;
	freq = freq;
	mag = mag;
	phase = phase }
	
    let mag t = t.mag
      
    (* based on the identities:
       cos(x+y) = cosx * cosy - sinx * siny
       sin(x+y) = sinx * cosy + cosx * siny
    *)
    let real t f =
      if f = t.freq then begin
	match t.source_type with
	  | SIN -> t.mag *. (sin t.phase)
	  | COS -> t.mag *. (cos t.phase)
      end else 0.
	
    let imag t f =
      if f = t.freq then begin
	match t.source_type with
	  | SIN -> t.mag *. (sin t.phase)
	  | COS -> -.t.mag *. (sin t.phase)
      end else 0.
	
    let value t time =
      (* FIXME: this is not exactly right... ignores phase *)
      match t.source_type with
	| SIN -> failwith "not supported as of yet"
	| COS -> t.mag *. (cos (t.freq *. time))
	    
    let deriv t time =
      (* FIXME: this is not exactly right... ignores phase *)
      match t.source_type with
	| SIN -> failwith "not supported as of yet"
	| COS -> -.t.mag *. (sin (t.freq *. time))
	    
    let zero = vac ~freq:0. ~mag:0. ()

    let negate t = { t with mag = -.1. *. t.mag }
  end

  module DC = struct

    module I = Gsl_interp
      
    type source_type =
	DC of mag
      | PWL of period * I.interp
    and time = float
    and mag = float
    and period = float
    and source = {
      source_type : source_type;
    }
	
    let pwl (vals: (time*mag) list) =
      let x,y = List.split vals in
      let x,y = (Array.of_list x), (Array.of_list y) in
      let period = x.((Array.length x) - 1) in
      printf "PWL period=%g\n%!" period;
      let pwl =
	PWL( period, (I.make_interp I.LINEAR x y)) in
      { source_type = pwl }
	
    let vdc mag = { source_type=DC mag }

    let zero = vdc 0.

    let value t time = match t.source_type with
      | DC mag -> mag
      | PWL(period, i) ->
	  let x = I.eval i (mod_float time period) in
	  (*printf "pwl time=%g val=%g\n%!" time x;*)
	  x

    let deriv t time = match t.source_type with
      | DC _ -> 0.
      | PWL(period, i) -> I.eval_deriv i (mod_float time period)

    let negate t = match t.source_type with
      | DC mag -> vdc (-.1. *. mag)
      | PWL(period, i) ->
	  let pwl = I.make_interp I.LINEAR i.I.xa
	    (Array.map (fun x -> -.1. *. x) i.I.ya) in
	  {source_type = PWL(period, pwl)}
      
  end

  let eval id ac dc (src_input:source_func_input) =
    match src_input with
      | `DC time -> DC.value dc time
	  (*+. (value ac time)*)
      | `DC_SWEEP (set_id, set_value) -> (* FIXME: should we take time here *)
	  if set_id = id then set_value else DC.value dc 0.
      | `DC_DERIV time ->
	  (AC.deriv ac time) +. (DC.deriv dc time)
      | `AC_MAG -> AC.mag ac
      | `AC_REAL freq -> AC.real ac freq
      | `AC_IMAG freq -> AC.imag ac freq
    
end;;
    
let node_count = 20
let create_circuit () = {
  node_map     = Hashtbl.create node_count;
  source_map   = Hashtbl.create node_count;
  g_map        = Hashtbl.create (node_count * node_count);
  c_map        = Hashtbl.create (node_count * node_count);
  b_map        = Hashtbl.create node_count;
  f_map        = Hashtbl.create node_count;
  x_array      = DynArray.make node_count;
  subckt_index = 0;
  subckt_p     = false;
}

(* Adds a new node to the set of equations and returns the internal and
   external label as the pair (external_id, internal_id)
*)
let add_node (c:circuit) ?node () =
  let new_node_id = Hashtbl.length c.node_map in
  DynArray.add c.x_array Voltage;
  match node with
    | Some x ->
	let mapped_node = if c.subckt_p then
	  c.subckt_index * 100 + x else x
	in
        (*printf "add_node mapping: %d %d\n%!" x new_node_id;*)
        Hashtbl.add c.node_map mapped_node new_node_id;
        mapped_node, new_node_id
    | None ->
        (*printf "add_node mapping2: %d %d\n%!" new_node_id
  new_node_id;*)
	let mapped_node = if c.subckt_p then
	  c.subckt_index * 100 + new_node_id else new_node_id
	in
        Hashtbl.add c.node_map mapped_node new_node_id;
        mapped_node, new_node_id

(*let add_subckt_nodes c node_list =
  c.subckt_index <- c.subckt_index + 1;
  List.iter (fun n ->*)
    
	  
(* Takes and external node label and returns the internal label *)
let map_node (c:circuit) node =
  if node = 0 then `Ground else
    let mapped_node = if c.subckt_p then
      c.subckt_index * 100 + node else node
    in
    if Hashtbl.exists c.node_map mapped_node then (
      debug "found node: %d --> %d" node (Hashtbl.find c.node_map node);
      `Node (Hashtbl.find c.node_map mapped_node))
    else
      let _, id = add_node c ~node:node () in
      `Node id

let find_branch c node = Hashtbl.find c.node_map node
let find_node c node = Hashtbl.find c.node_map node

(* Adds an admittance of size value between nodes n1 and n2 *)
let add_admittance (c:circuit) (value:admittance) n1 n2 =
  (* printf "add_admittance: %d,%d --> %g\n" n1 n2 value; flush stdout; *)
  let n1, n2 = (map_node c n1), (map_node c n2)
  and admittance_map, value = match value with
    | `Y x -> c.c_map, x | `G x -> c.g_map, x in
  let add_entry n value =
    Hashtbl.replace admittance_map n
      ((Hashtbl.find_default admittance_map n 0.0) +. value)
  in
    (*printf "add_admittance: %d,%d %g\n" n1 n2 value;*)
    match n1,n2 with
      | `Node n1, `Node n2 ->
          add_entry (n1,n1) value;
          add_entry (n2,n2) value;
          add_entry (n1,n2) (-.value);
          add_entry (n2,n1) (-.value);
      | `Node n1, `Ground ->
          add_entry (n1,n1) value;
      | `Ground, `Node n2 ->
          add_entry (n2,n2) value
      | `Ground, `Ground -> ()

let add_branch (c:circuit) id ?value:(value=1.0) n1 n2 =
  if Hashtbl.exists c.node_map id then failwith
    (sprintf "ID %d already exists" id);
  let n1, n2 = (map_node c n1), (map_node c n2)
  and _, n3 = add_node c ~node:id ()
  and set = Hashtbl.replace c.g_map in
    (match n1,n2 with
      | `Ground, `Node n2 ->
          set (n2, n3) value;
          set (n3, n2) value;
      | `Node n1, `Ground ->
          set (n1, n3) value;
          set (n3, n1) value;
      | `Node n1, `Node n2 ->
          set (n1, n3) value;
          set (n3, n1) value;
          set (n2, n3) (-.value);
          set (n3, n2) (-.value);
      | `Ground, `Ground -> ());
    DynArray.add c.x_array Current;
    n3

let add_resistor c n1 n2 r_val =
  add_admittance c (`G (1.0 /. r_val)) n1 n2

let add_capacitor c n1 n2 c_val =
  add_admittance c (`Y c_val) n1 n2

let add_inductor c ~id n1 n2 l =
  if l < 0. then failwith "Invalid inductance value";
  let n3 = add_branch c id n1 n2 in
    Hashtbl.replace c.c_map (n3, n3) (-.l)

(* The DC source function takes an ID and value which is used during
   DC sweeps.
   The AC source function is slighly more complicated... one may ask for
   the real/imag components of the source (fft components for HB),
   or simply the magnitude (linear AC analysis).
*)
let add_voltage_source c ~id ?dc:(dc=Source.DC.zero)
    ?ac:(ac=Source.AC.zero) n1 n2 =
  let n3 = add_branch c id n1 n2 in
  Hashtbl.replace c.source_map id (`VSRC n3);
  let cur_value = Hashtbl.find_default c.b_map n3 [] in
  (* FIXME: this is actually a type of error *)
  Hashtbl.replace c.b_map n3 ((Source.eval id ac dc) :: cur_value)

(* n1 is the node where current is "pushed in"
   n2 is the node where current is "sucked from" *)
let add_current_source c ~id ?dc:(dc=Source.DC.zero)
    ?ac:(ac=Source.AC.zero) n1 n2 =
  (* FIXME: should't we add these.. multiple current sources *)
  let n1, n2 = (map_node c n1), (map_node c n2) in
  Hashtbl.replace c.source_map id (`ISRC (n1,n2));
  (match n1 with
    | `Node n1 ->
	let cur_value = Hashtbl.find_default c.b_map n1 [] in
        Hashtbl.replace c.b_map n1 ((Source.eval id ac dc) :: cur_value)
    | `Ground -> ());
  match n2 with
    | `Node n2 ->
	let cur_value = Hashtbl.find_default c.b_map n2 [] in
        Hashtbl.replace c.b_map n2
	  ((Source.eval id (Source.AC.negate ac) (Source.DC.negate dc)) ::
	     cur_value)
    | `Ground -> ()
	
(* nonlinear current source:
   - current flows into node "n2"
   - fx takes a vector of voltages and returns the current
   - dfx takes a vector of voltages and fills in a row of the jacobian *)
let add_nonlinear_isrc c n1 n2 ~fx ?(dfx:unsigned_df_type option) () =
  let n1, n2 = (map_node c n1), (map_node c n2) in
  let _ = match n1 with
    | `Node n -> sprintf "%d" n | `Ground -> "GND"
  and _ = match n2 with
    | `Node n -> sprintf "%d" n | `Ground -> "GND"
  in
  (match n1 with
    | `Node n1 ->
	let dfx' = match dfx with
	  | None -> None
	  | Some fn -> Some (fn `POS)
	in
	Hashtbl.replace c.f_map n1
	  ((fx, dfx') :: (Hashtbl.find_default c.f_map n1 []))
    | `Ground -> ());
  match n2 with
    | `Node n2 ->
	let fx' = (fun ~x -> -1. *. (fx ~x)) in
	let dfx' = match dfx with
	  | None -> None
	  | Some fn -> Some (fn `NEG)
	in
        Hashtbl.replace c.f_map n2
	  ((fx', dfx') :: (Hashtbl.find_default c.f_map n2 []))
    | `Ground -> ()
	  
let add_vcvs c ~id ~ref1_id ~ref2_id ~gain n1 n2 =
  (* (n1 - n2) <= gain * (ref_id1 - ref_id2) *)
  let ref1_id, ref2_id = (map_node c ref1_id), (map_node c ref2_id) in
  let branch_id = add_branch c ~value:(1. /. gain) id n1 n2 in
    (match ref1_id with
      | `Node id1 -> Hashtbl.replace c.g_map (branch_id, id1) (-.1.)
      | `Ground -> ());
    (match ref2_id with
      | `Node id2 -> Hashtbl.replace c.g_map (branch_id, id2) 1.
      | `Ground -> ())

let add_vccs c ~id ~ref1_id ~ref2_id ~gain n1 n2 =
  (* n1 - n2 <= gain * (ref_id1 - ref_id2) *)
  debug "add_vccs";
  let add_entry (n1,n2) value =
    debug "  - add_entry %d,%d %g" n1 n2 value;
    Hashtbl.replace c.g_map (n1,n2)
      ((Hashtbl.find_default c.g_map (n1,n2) 0.0) +. value)
  in
  let ref1, ref2, n1, n2 = (map_node c ref1_id), (map_node c ref2_id),
    (map_node c n1), (map_node c n2) in
    (match ref1 with
      | `Ground -> ()
      | `Node ref1 ->
          (match n1 with
            | `Ground -> ()
            | `Node n1 -> add_entry(n1, ref1) gain);
          (match n2 with
            | `Ground -> ()
            | `Node n2 -> add_entry(n2, ref1) (-.gain)));
    (match ref2 with
      | `Ground -> ()
      | `Node ref2 ->
          (match n1 with
            | `Ground -> ()
            | `Node n1 -> add_entry(n1, ref2) (-.gain));
          (match n2 with
            | `Ground -> ()
            | `Node n2 -> add_entry(n2, ref2) gain))

let add_cccs c ~id ~gain ~ref1_id ~ref2_id n1 n2 =
  (* n1 -> n2 <= gain *. I(ref_id1 -> ref_id2) *)
  let id' = add_branch c id ref1_id ref2_id
  and n1, n2 = (map_node c n1), (map_node c n2) in
  (match n1 with
     | `Ground -> ()
     | `Node n1 ->
	 Hashtbl.replace c.g_map (n1, id') gain);
  (match n2 with
     | `Ground -> ()
     | `Node n2 ->
	 Hashtbl.replace c.g_map (n2, id') (-.1. *. gain))

let add_ccvs c ~id ~gain ~ref_id n1 n2 =
  (* gain: transconductance of the device
     ref_id: ID of the device for a reference current *)
  failwith "This is not yet working properly";
  if not (Hashtbl.exists c.node_map ref_id) then
    failwith (sprintf "Referenced device id=%d does not exist" ref_id);
  let branch_id = add_branch c id n1 n2
  and n1, n2, ref_id = (map_node c n1), (map_node c n2), (find_branch c ref_id) in
    (*Hashtbl.replace c.g_map (branch_id, ref_id) (-.gain);*)
    Hashtbl.replace c.g_map (ref_id, branch_id) (-.gain);
    (match n1 with
      | `Node n1 ->
          Hashtbl.replace c.g_map (n1, branch_id) (-.1.);
          Hashtbl.replace c.g_map (branch_id, n1) 1.;
      | `Ground -> ());
    (match n2 with
      | `Node n2 ->
          Hashtbl.replace c.g_map (n2, branch_id) 1.;
          Hashtbl.replace c.g_map (branch_id, n2) (-.1.);
      | `Ground -> ())












