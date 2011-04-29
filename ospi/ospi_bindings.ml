
module CircuitLib = struct
  open ExtHashtbl
  open Printf
    
  type component = RES | IND | CAP | VSRC | ISRC
  type id_data = {
    id_map             : (string, int) Hashtbl.t;
    circuit            : Circuit.circuit;
    recorder           : Recorder.recorder;
    mutable count      : int;
    mutable res_count  : int;
    mutable cap_count  : int;
    mutable ind_count  : int;
    mutable vsrc_count : int;
    mutable isrc_count : int;
  }

  let component_prefix = function
      RES -> "R"
    | IND -> "L"
    | CAP -> "C"
    | VSRC -> "V"
    | ISRC -> "I"

  let create () =
    printf "creating new circuit\n%!";
    let cir = Circuit.create_circuit () in
    let r = new Recorder.recorder cir in
    {
      id_map     = Hashtbl.create 10;
      circuit    = cir;
      recorder   = r;
      count      = 0;
      res_count  = 0;
      cap_count  = 0;
      ind_count  = 0;
      vsrc_count = 0;
      isrc_count = 0;
    }

  let new_id t ?id ctype =
    t.count <- t.count + 1;
    let next_id = match ctype with
	RES -> t.res_count <- t.res_count + 1; t.res_count
      | IND -> t.ind_count <- t.ind_count + 1; t.ind_count
      | CAP -> t.cap_count <- t.cap_count + 1; t.cap_count
      | VSRC -> t.vsrc_count <- t.vsrc_count + 1; t.vsrc_count
      | ISRC -> t.isrc_count <- t.isrc_count + 1; t.isrc_count
    in
    let () = match id with
	Some id ->
	  if Hashtbl.exists t.id_map id then failwith "id already exists";
	  Hashtbl.replace t.id_map id t.count;
      | None ->
	  let prefix = component_prefix ctype in
	  Hashtbl.replace t.id_map (sprintf "%s%d" prefix next_id)
	    t.count;
    in
    t.count
	    
end

module Bindings = struct
  open Ocs_types
  open Ocs_error
  open Circuit
  open Engine
  open Printf
  open CircuitLib
    
  let sfloat s =
    try
      Ocs_utilities.float_of_sval s
    with
        Invalid_argument s -> exprintf "%s cannot be converted to real" s

  let sint s =
    try
      Ocs_utilities.int_of_sval s
    with
        Invalid_argument s -> exprintf "%s cannot be converted to integer" s
	    
  let rec spair_to_list = function
      {car=n; cdr=Snull} -> [n]
    | {car=n; cdr=Spair p} -> n :: spair_to_list p
    | _ -> exprintf "par:arr: expects proper list"

  let psv f s = Format.fprintf f "%s" (Ocs_print.string_of_sval s)

  let now () = Sreal (Unix.gettimeofday())

  let lib = create ()

  let add_res = function
      [| Ssymbol id; Sint n1; Sint n2; Sreal value |] ->
	add_resistor lib.circuit n1 n2 value; Sunspec
    | [| Sint n1; Sint n2; Sreal value |] ->
	add_resistor lib.circuit n1 n2 value; Sunspec
    | _ -> Ocs_error.exprintf "spi:res: bad args"
	
  let add_cap = function
      [| Ssymbol id; Sint n1; Sint n2; Sreal value |] ->
	add_capacitor lib.circuit n1 n2 value; Sunspec
    | [| Sint n1; Sint n2; Sreal value |] ->
	add_capacitor lib.circuit n1 n2 value; Sunspec
    | _ -> Ocs_error.exprintf "spi:cap: bad args"
      
  let add_ind = function
      [| Ssymbol id; Sint n1; Sint n2; Sreal value |] ->
	let id = new_id lib ~id IND in
	add_inductor lib.circuit ~id n1 n2 value; Sunspec
    | [| Sint n1; Sint n2; Sreal value |] ->
	let id = new_id lib IND in
	add_inductor lib.circuit ~id n1 n2 value; Sunspec
    | _ -> Ocs_error.exprintf "spi:ind: bad args"
      
  let add_vsrc args kwargs =
    let get s = List.map (fun s -> List.assoc s kwargs) s in
    try
      let ac, dc = match get ["ac:"; "dc:"] with
	  [ _; Sreal dc] -> printf "dc baby: %g\n%!" dc; None, Some (Source.DC.vdc dc)
	| _ -> Ocs_error.exprintf "spi:vsrc: bad args"
      in
      match args with
	  [| Ssymbol id; Sint n1; Sint n2 |] ->
	    let id = new_id lib ~id VSRC in
	    add_voltage_source lib.circuit ~id ?ac ?dc n1 n2; Sunspec
	| [| Sint n1; Sint n2 |] ->
	    let id = new_id lib VSRC in
	    add_voltage_source lib.circuit ~id ?ac ?dc n1 n2; Sunspec
	| _ -> Ocs_error.exprintf "spi:vsrc: bad args";
    with _ -> Ocs_error.exprintf "spi:vsrc: bad args"

  let run_dc name = match name with
      Ssymbol name -> 
	let solver = GslEngine.Builder.create lib.circuit in
	GslEngine.DC.solve ~name solver lib.recorder;
	Sunspec
    | _ -> Ocs_error.exprintf "spi:dc: bad args"

  let vdc name node = match name, node with
      Ssymbol name, Sint node-> begin
	match lib.recorder#val_at name node 0. with
	  | `Complex x -> failwith "unsupported type from DC analysis"
	  | `Real x -> Sreal x
      end
    | _ -> Ocs_error.exprintf "spi:vdc: bad args"

  let add_isrc args kwargs = (match args with
    | _ -> ();
  );
    Sunspec
      
  let init e =
    Ocs_env.set_pf1  e Std.identity "identity";
    Ocs_env.set_pf0  e now     "now";
    Ocs_env.set_pf1  e run_dc  "spi:dc";
    Ocs_env.set_pf2  e vdc     "spi:vdc";
    Ocs_env.set_pfn  e add_res "spi:res";
    Ocs_env.set_pfn  e add_cap "spi:cap";
    Ocs_env.set_pfn  e add_ind "spi:ind";
    Ocs_env.set_pfn  e (Ocs_env.make_decoder add_vsrc 3 []
			  [("dc:", Sreal 0.); ("ac:", Sreal 0.)]) "spi:vsrc";
    Ocs_env.set_pfn  e (Ocs_env.make_decoder add_isrc 3 []
			  [("dc:", Sreal 0.); ("ac:", Sreal 0.)]) "spi:isrc";
  ;;
  
end
