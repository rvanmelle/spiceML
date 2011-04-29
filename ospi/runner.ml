open Circuit
open Recorder
open Engine
open Gsl_helpers
open Printf

type diode_model = {
  isat : float;
  vt   : float
}

type dc_solver = [ `DERIV | `NODERIV]
type analysis_type = [ `DC_SWEEP | `AC_SWEEP | `HB_TIME | `TRAN | `SM ]
    
class circuit_driver (cir:circuit) (r:recorder) = 
  
object (self)

  (* returns the value corresponding to the named external node *)
  method v (voltages:Gsl_vector.vector) node = match (map_node cir node) with
    | `Ground -> 0.
    | `Node loc -> voltages.{loc}

  (* returns the internal node id for the named external node *)
  method index node : circuit_node = map_node cir node
	
  method res n1 n2 ~r =
    add_resistor cir n1 n2 r
  method cap n1 n2 ~c =
    add_capacitor cir n1 n2 c
  method ind ~id n1 n2 ~(l:float) =
    add_inductor cir ~id n1 n2 l
  method vsrc ~id ?dc ?ac n1 n2 =
    add_voltage_source cir ~id ?dc ?ac n1 n2
  method isrc ?dc ?ac n1 n2 =
    add_current_source cir ?dc ?ac n1 n2
  method cccs ~id ~ref1 ~ref2 ~gain n1 n2 =
    add_cccs cir ~id ~ref1_id:ref1 ~ref2_id:ref2 ~gain n1 n2
      
  (* a bsrc is a non-linear current source *)
  method bsrc n1 n2 ~(fx:f_type)
    ?(dfx:unsigned_df_type option) () =
    add_nonlinear_isrc cir n1 n2 ~fx ?dfx ()

  (* Non-linear DC analysis routines *)
  method dc name =
    let solver = GslEngine.Builder.create cir in
    GslEngine.DC.solve ~name solver r
  method dc_sweep name ~id ~start ~stop ~steps =
    let solver = GslEngine.Builder.create cir in
    let spec = GslEngine.DC.create_sweep name id start stop steps in
    GslEngine.DC.sweep solver r spec

  (* Volterra-based method of non-linear currents *)
  method volterra (name:string) ~src1 ~src2 ~conductance =
    let solver = GslEngine.Builder.create cir in
    GslEngine.DC.solve ~name:"dc" solver r;
    let xinit = GslEngine.Solver.get_solution solver in
    GslEngine.Volterra.solve name solver ~xinit ~src1 ~src2 ~conductance
    
  (* Harmonic Balance analysis routines *)
  method multiHB (name:string) ~k1 ~k2 ~w1 ~w2 =
    let solver = GslEngine.Builder.create cir in
    GslEngine.DC.solve ~name:"dc" solver r;
    (*let hb = GslEngine.MultiToneHB.create solver
      ~xinit:(GslEngine.Solver.get_solution solver) ~k1 ~w1 ~k2 ~w2 in
    GslEngine.MultiToneHB.solve ~name hb solver r*)
      
    let hb = GslEngine.HB.create_two_tone solver
      ~xinit:(GslEngine.Solver.get_solution solver) ~k1 ~w1 ~k2 ~w2 in
    GslEngine.HB.solve ~name hb solver r
      
  (* Harmonic Balance analysis routines *)
  method hb name ~harmonics ~w =
    let solver = GslEngine.Builder.create cir in
    GslEngine.DC.solve ~name:"dc" solver r;
    let hb = GslEngine.HB.create_single_tone solver
      ~xinit:(GslEngine.Solver.get_solution solver) ~size:harmonics ~w in
    GslEngine.HB.solve ~name hb solver r
  method hb_linear (name:string) ~harmonics ~w =
    let solver = GslEngine.Builder.create cir in
    GslEngine.DC.solve ~name:"dc" solver r;
    let hb = GslEngine.HB.create_single_tone solver
      ~xinit:(GslEngine.Solver.get_solution solver) ~size:harmonics ~w in
    GslEngine.HB.solve_linear ~name hb r

  (*Shooting method routines *)
  method sm name period =
    let solver = GslEngine.Builder.create cir in
    GslEngine.DC.solve ~name:"sm" solver r;
    let sm = GslEngine.ShootingMethod.create name period in
    let xinit = V.of_array (GslEngine.Solver.get_solution solver) in
    GslEngine.ShootingMethod.run sm ~xinit solver r
      
  (* Non-linear transient analysis *)
  method tran (name:string) ~duration =
    let solver = GslEngine.Builder.create cir in
    let tran = GslEngine.Tran.create name duration in
    GslEngine.DC.solve ~name:"dc" solver r;
    GslEngine.Tran.run tran solver r
      ~xinit:(GslEngine.Solver.get_solution solver) 
    
  (* Linear AC analysis routines *)
  method ac name ~f =
    let solver = GslEngine.Builder.create cir in
    GslEngine.AC.solve_at ~name solver r f
  method ac_sweep name ~(src:int) ~start ~stop ~steps =
    let solver = GslEngine.Builder.create cir in
    let ac = GslEngine.AC.create_log name start stop steps in
    GslEngine.AC.run ac solver r

  (* General Output *)
  method html_of_mna file =
    let solver = GslEngine.Builder.create cir in
    GslEngine.Solver.print ~file solver

  method print_table ~file name node_ids =
    r#html_of_signals ~file name node_ids
      
  (* Plotting *)
  method private get_signals ?(named=[]) (anal_type:analysis_type) name ids =
    let signals, logx, logy = match anal_type with
      | `HB_TIME ->
	  let signals = List.map (fun id ->
	    match r#get_ifft name id with
	      | `Complex value -> failwith "that is a surprise"
	      | `Real value ->
		  let name = sprintf "v(%d)" id in
		  Ospi_waveform.new_signal ~name ~trace:`LinesPoints 
		    ~lines:`Solid ~points:`None value;
	  ) ids in
	  signals, false, false
      | `TRAN | `SM ->
	  let signals = List.map (fun id ->
	    match r#get_signal name id with
	      | `Complex value -> failwith "that is a surprise"
	      | `Real value ->
		  let name = sprintf "v(%d)" id in
		  Ospi_waveform.new_signal ~name ~trace:`LinesPoints 
		    ~lines:`Solid ~points:`None value;
	  ) ids in
	  signals, false, false
      |	`DC_SWEEP ->
	  let signals = List.map (fun id ->
	    match r#get_signal name id with
	      | `Complex value -> failwith "that is a surprise"
	      | `Real value ->
		  let name = sprintf "v(%d)" id in
		  Ospi_waveform.new_signal ~name ~trace:`LinesPoints 
		    ~lines:`Solid ~points:`None value;
	  ) ids in
	  signals, false, false
      | `AC_SWEEP ->
	  let signals = List.map (fun id ->
	    match r#get_signal name id with
	      | `Real value -> failwith "this is a surprise"
	      | `Complex value ->
		  match Functions.mag (Value.COMPLEX_SIGNAL value) with
		    | Value.REAL_SIGNAL x ->
			let name = sprintf "v(%d)" id in
			Ospi_waveform.new_signal ~name ~trace:`LinesPoints 
			  ~lines:`Solid ~points:`None x;
		    | _ -> failwith "Unexpected result"
	  ) ids in
	  signals, true, false
    in
    let named_signals = List.map
      (fun id -> match r#get_named_signal id with
	 | `Complex value -> failwith "that is weird"
	 | `Real value -> Ospi_waveform.new_signal ~name:id ~trace:`LinesPoints 
	     ~lines:`Solid ~points:`None value;
      ) named in
    (List.append signals named_signals), logx, logy

    
  method plot ?named anal_type name ids =
    let signals, logx, logy = self#get_signals ?named anal_type name ids in
    Plotter.plot ~logx ~logy signals

  method print ?named anal_type name ids ~file =
    let signals, logx, logy = self#get_signals ?named anal_type name ids in
    let outc = open_out file in
    Plotter.print signals ~outc ~format:`SVG
      ~logx ~logy ~width:500 ~height:350;
    close_out outc

  (* Data input/output *)
  method load name file = r#load name file
      
  (* Value extraction *)
  method vdc name n =
    match r#val_at name n 0. with
      | `Complex x -> failwith "unsupported type from DC analysis"
      | `Real x -> x
  method vac name n ~f = r#mag_at name n f
  method vm name n ~f = r#mag_at name n f
  method vp name n ~f = r#phase_at name n f
end

(* Convenience routine for creating a new circuit *)
let new_circuit () =
  let c = create_circuit () in
  let r = new recorder c in
  new circuit_driver c r
