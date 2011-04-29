open Circuit
open Runner
open Printf
open Circuit_models

module Diode = Circuit_models.Diode

let test_circuit_sm () =
  let c = new_circuit () in
  c#isrc 1 0 ~id:50 ~dc:
    (Source.DC.pwl [(0., 0.); (1e-9, 0.5); ((0.5e-3 -. 1e-9), 0.5);
		    (0.5e-3, 0.); (1e-3, 0.)]);
  c#res 1 0 ~r:2.;
  c#cap 1 0 ~c:0.2e-6;
  Diode.diode_subckt c ~nb:1 ~ne:2;
  c#cap 2 0 ~c:0.2e-3;
  c#res 2 0 ~r:100.;
  c#sm "SM" 0.001;
  c#load "ADS v(out)" "ospi/assign5/hb_vout.csv";
  c#print ~named:["ADS v(out)"] `SM "SM" [2] ~file:"sm.svg";
  c#plot ~named:["ADS v(out)"] `SM "SM" [2]

let test_circuit_tran1 () =
  let c = new_circuit () in
  c#isrc 1 0 ~id:50 ~dc:
    (Source.DC.pwl [(0., 0.); (1e-9, 0.5); ((0.5e-3 -. 1e-9), 0.5);
		    (0.5e-3, 0.); (1e-3, 0.)]);
  c#res 1 0 ~r:2.;
  c#cap 1 0 ~c:0.2e-6;
  Diode.diode_subckt c ~nb:1 ~ne:2;
  c#cap 2 0 ~c:0.2e-3;
  c#res 2 0 ~r:100.;
  c#load "ADS v(out)" "ospi/assign5/tran_vout.csv";
  c#tran "Tran" 0.01;
  c#print ~named:["ADS v(out)"] `TRAN "Tran" [2] ~file:"tran.svg";
  c#plot ~named:["ADS v(out)"] `TRAN "Tran" [2]
  
    
let test_circuit_tran2 () =
  let c = new_circuit () in
  c#vsrc 3 0 ~id:50 ~dc:
    (Source.DC.pwl [(0., 0.); (1e-9, 1.0); ((0.5e-3 -. 1e-9), 1.0);
		    (0.5e-3, 0.); (1e-3, 0.)]);
  c#res 3 1 ~r:2.;
  c#cap 1 0 ~c:0.2e-6;
  Diode.diode_subckt c ~nb:1 ~ne:2;
  c#cap 2 0 ~c:0.2e-3;
  c#res 2 0 ~r:10.;
  c#tran "Tran" 0.01;
  c#plot `TRAN "Tran" [1;2]

let _ = test_circuit_sm ()

  
  
		 
