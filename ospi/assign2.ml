open Engine
open Recorder
open Circuit
open Runner
open Printf
open Util.MathHelpers
open Circuit_models

module AC = Source.AC
module DC = Source.DC

let test_circuit_hb mag harmonics =
  let c = new_circuit () in
  let vcc, rsig, re, rc, rl, c1, c2 =
    15., 2.5e3, 28.57e3, 20e3, 10e3, 20e-3, 20e-3 in
  let f = 2. *. pi *. 10e3 in
  c#vsrc 1 0 ~id:50 ~ac:
    (AC.vac ~src_type:AC.COS ~freq:f ~mag ~phase:(pi /. 6.) ());
  c#vsrc 4 0 ~id:51 ~dc:(DC.vdc vcc);
  c#vsrc 7 0 ~id:52 ~dc:(DC.vdc (-.vcc));
  c#res 1 2 ~r:rsig;
  c#res 3 4 ~r:rc;
  c#res 6 7 ~r:re;
  c#cap 6 0 ~c:c2;
  c#cap 3 5 ~c:c1;
  c#res 5 0 ~r:rl;
  BJT.bjt_subckt c ~nb:2 ~nc:3 ~ne:6 ~ns:0;
  c#dc "DC";
  c#hb "HB" ~harmonics ~w:f;
  let f1 = f and f2 = 2. *. f and f3 = 3. *. f in
  printf "DC shift: %g\n" (c#vm "HB" 5 ~f:0.);
  printf "Gain: %g\n" (20. *. log10 ((c#vm "HB" 5 ~f:f1) /. (c#vm "HB" 1 ~f:f1)));
  printf "2nd HD: %g\n" (20. *. log10 ((c#vm "HB" 5 ~f:f2) /. (c#vm "HB" 5 ~f:f1)));
  printf "3rd HD: %g\n" (20. *. log10 ((c#vm "HB" 5 ~f:f3) /. (c#vm "HB" 5 ~f:f1)));
  let sum = ref 0. in
  for i = 2 to harmonics do
    let mag = (c#vm "HB" 5 ~f:((float i) *. f)) in
    sum := !sum +. (mag ** 2.)
  done;
  let mag1 = c#vm "HB" 5 ~f:f1 in
  printf "|Vout|: %g\n" mag1;
  printf "THD: %g\n" (20. *. log10 (!sum /. mag1));
    
  c#print `HB_TIME "HB" [5] ~file:"v_5.svg"
  (*c#plot `HB_TIME "HB" [1];*)
  (*c#plot `HB_TIME "HB" [5]*)
  
(*let test_circuit_tran () =
  let c = new_circuit () in
  let vcc, rsig, re, rc, rl, c1, c2 =
    15., 2.5e3, 28.57e3, 20e3, 10e3, 20e-3, 20e-3 in
  c#vsrc 1 0 ~id:50 ~dc:0. ~ac:
    (AC.vac ~src_type:`COS ~freq:10e3 ~mag:0.1 ~phase:(pi /. 6.) ());
  c#vsrc 4 0 ~id:51 ~dc:vcc;
  c#vsrc 7 0 ~id:52 ~dc:(-.vcc);
  c#res 1 2 ~r:rsig;
  c#res 3 4 ~r:rc;
  c#res 6 7 ~r:re;
  c#cap 6 0 ~c:c2;
  c#cap 3 5 ~c:c1;
  c#res 5 0 ~r:rl;
  BJT.bjt_subckt c ~nb:2 ~nc:3 ~ne:6 ~ns:0;
  c#tran "Tran" 0.1;
  c#plot `TRAN "Tran" [1; 5]*)

let compare_vout () =
  let sweep = Value.farray_of_array
    [| 1e-3; 5e-3; 10e-3; 50e-3; 0.1; 0.5 |] in
  let my_data = Value.farray_of_array
    [| 0.07468; 0.373; 0.7433; 3.299; 4.7024; 4.997 |]
  and ads_data = Value.farray_of_array
    [| 0.075; 0.373; 0.743; 3.291; 4.715; 5.133 |]
  in
  let my_sig = { Value.real_sweep=sweep; Value.real_trace=my_data}
  and ads_sig = { Value.real_sweep=sweep; Value.real_trace=ads_data} in
  let mine = Ospi_waveform.new_signal ~trace:`Points ~points:`Circle my_sig
  and ads = Ospi_waveform.new_signal ~trace:`Points ~points:`Cross ads_sig
  in
    Plotter.print ~format:`SVG ~outc:(open_out "compare.svg")
      ~logx:false ~logy:false ~width:400 ~height:400 [mine; ads]
    
let main () =
  Gsl_error.init () ;
  (*compare_vout ()*)
  (*test_circuit_tran ()*)
  if false then 
    test_circuit_hb 100e-3 20
  else
    test_circuit_hb 5e-3 5
  (*test_circuit_hb 5e-3 5;
  test_circuit_hb 10e-3 5;
  test_circuit_hb 50e-3 7;
  test_circuit_hb 100e-3 20;*)
  (*test_circuit_hb 500e-3 20*)

let _ = main ()
  

