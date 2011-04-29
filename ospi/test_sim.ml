open OUnit
open Engine
open Recorder
open Circuit
open Runner
open Printf
open Util.TestHelpers

let pi = Util.MathHelpers.pi

let make_circuit (build_cb: Circuit.circuit -> unit) =
  let c = create_circuit () in
    build_cb c;

    let solver = GslEngine.Builder.create c in
    let r = new recorder c in
      GslEngine.AC.solve_at solver r 100.;
      r

let fcmp a b =
  assert_equal ~cmp:(OUnit.cmp_float ~epsilon:0.001)
    ~printer:string_of_float a b

module BJT = 
  Circuit_models.BJT(struct let beta = 100. and io = 1e-15 and vt = 25e-3 end);;
module AC = Source.AC
module DC = Source.DC

let suite = "ospi" >::: [
  "dc" >::: [

    "simple_nonlinear_isrc" >::
      (fun () ->
	let c = new_circuit () in
	c#vsrc 1 0 ~id:100 ~dc:(DC.vdc 0.8);
	c#res 1 2 ~r:1.;
	c#res 2 3 ~r:1.;
	c#bsrc 3 4 ~fx:(BJT.isrc_eq_f c#v 3 0 1)
	  ~dfx:(BJT.isrc_eq_df c#v c#index 3 0 1) ();
	c#res 4 0 ~r:1.;
	c#dc "DC";
	0.8 @=.? (c#vdc "DC" 1);
	0.739173 @=.? (c#vdc "DC" 2);
	0.678347 @=.? (c#vdc "DC" 3);
	0.0608265 @=.? (c#vdc "DC" 4);
	printf "!!!1=%g 2=%g 3=%g 4=%g\n" (c#vdc "DC" 1)
	  (c#vdc "DC" 2) (c#vdc "DC" 3) (c#vdc "DC" 4);
      );

    "simple_amplifier" >::
      (fun () ->
	 let c = new_circuit () in
	 let vcc, rsig, re, rc, rl, c1, c2 =
	   15., 2.5e3, 28.57e3, 20e3, 10e3, 20e-3, 20e-3 in
	 c#vsrc 1 0 ~id:50 ~dc:(DC.vdc 0.7); (* 0.7 *)
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
	 0.7 @=.? (c#vdc "DC" 1);
	 15. @=.? (c#vdc "DC" 4);
	 -.15. @=.? (c#vdc "DC" 7);
	 0.6869 @=.? (c#vdc "DC" 2);
	 4.5203 @=.? (c#vdc "DC" 3);
	 0.0 @=.? (c#vdc "DC" 5);
	 0.11995 @=.? (c#vdc "DC" 6);
      )
  ];

  (*"dc_sweep" >::: [
    "voltage_divider" >::
      (fun () ->
	 let c = new_circuit () in
	 c#vsrc 1 0 ~id:50 ~dc:(DC.vdc 5.);
	 c#res 1 2 ~r:1e3;
	 c#res 2 0 ~r:1e3;
	 c#dc_sweep "SW" ~id:50 ~start:(-.2.) ~stop:2. ~steps:10;
	 
      );
    (*c#dc_sweep "dc_sweep" ~id:50 ~start:(-.10.) ~stop:10. ~steps:30;
      c#plot `DC_SWEEP "dc_sweep" [1; 3; 6];
      c#plot `DC_SWEEP "dc_sweep" [50; 51; 52; 60]*)
  ];*)
  
  "ac" >::: [
    
    "voltage_divider_vsrc" >::
      (fun () ->
         let r =
           make_circuit
	     (fun c ->
		let ac = AC.vac ~freq:10e3 ~mag:10. () in	   
		 add_voltage_source c ~id:100 ~ac 1 0;
		 add_resistor c 4 0 20.;
		 add_resistor c 2 3 10.;
		 add_resistor c 3 4 10.;
		 add_resistor c 1 2 10.;
              )
          in
          10. @=.? (r#mag_at "ac" 1 100.);
          (10. *. 2. /. 5.) @=.? (r#mag_at "ac" 4 100.);
          (10. *. 3. /. 5.) @=.? (r#mag_at "ac" 3 100.);
          (10. *. 4. /. 5.) @=.? (r#mag_at "ac" 2 100.);
          (10. /. 50.) @=.? (r#mag_at "ac" 100 100.);
       );
     
     "subcircuit" >::
       (fun () ->
	  let f = 100. in
	  let vdivider c n1 n2 =
	    c#res n1 3001 ~r:10.;
	    c#res 3001 n2 ~r:10.;
	  in
	  let c = new_circuit () in
	  c#vsrc 1 0 ~id:50 ~ac:(AC.vac ~freq:1e3 ~mag:10. ());
	  c#res 1 2 ~r:10.;
	  vdivider c 2 3;
	  c#res 3 0 ~r:20.;
	  c#ac "AC" ~f;
	  10. @=.? (c#vac "AC" 1 ~f);
          (10. *. 2. /. 5.) @=.? (c#vac "AC" 3 ~f);
          (10. /. 50.) @=.? (c#vac "AC" 50 ~f);
       );
     
     "voltage_divider_isrc" >::
       (fun () ->
          let r =
            make_circuit
	      (fun c ->
		 let ac = AC.vac ~freq:1e3 ~mag:1.5 () in
		 add_current_source c ~id:50 ~ac 2 0;
		 add_resistor c 2 3 5.;
		 add_resistor c 3 0 2.;
              )
          in
          3. @=.? (r#mag_at "ac" 3 100.);
          10.5 @=.? (r#mag_at "ac" 2 100.);
       );

     "summing current sources" >::
       (fun () ->
	  let c = new_circuit () in
	  let dc1 = DC.vdc 0.5 and dc2 = DC.vdc 0.6 in
	  c#isrc 1 0 ~id:50 ~dc:dc1;
	  c#isrc 1 0 ~id:51 ~dc:dc2;
	  c#res 1 0 ~r:5.;
	  c#dc "DC";
	  (5. *. 1.1) @=.? (c#vdc "DC" 1);
       );
     
     "capacitors" >::
       (fun () ->
	 let freq = 2e3 in
	 let c = new_circuit () in
	 c#vsrc 1 0 ~id:50 ~ac:(AC.vac ~freq ~mag:1. ());
	 c#cap 1 2 ~c:1e-6;
	 c#cap 2 0 ~c:2e-6;
	 c#ac "AC" ~f:freq;
	 let z1 = 1. /. (2. *. pi *. freq *. 1e-6)
	 and z2 = 1. /. (2. *. pi *. freq *. 2e-6) in
	 1. *. (z2 /. (z1 +. z2)) @=.? (c#vac "AC" 2 freq);
	 1. /. (z1 +. z2) @=.? (c#vac "AC" 50 freq);
       );

     "inductors" >::
       (fun () ->
	  let freq = 100e3 in
	  let c = new_circuit () in
	  c#vsrc 1 0 ~id:50 ~ac:(AC.vac ~freq ~mag:1. ());
	  c#ind ~id:51 1 2 ~l:1e-9;
	  c#ind ~id:52 2 0 ~l:5e-9;
	  c#ac "AC" ~f:freq;
	  let z1 = 2. *. pi *. freq *. 1e-9
	  and z2 = 2. *. pi *. freq *. 5e-9 in
	  1. *. (z2 /. (z1 +. z2)) @=.? (c#vac "AC" 2 freq);
	  1. /. (z1 +. z2) @=.? (c#vac "AC" 50 freq);
       );
     
     "vcvs" >::
       (fun () ->
          let gain = 10. in
          let r =
            make_circuit
	      (fun c ->
		 add_voltage_source c ~id:100 ~ac:(AC.vac ~freq:1e3 ~mag:1. ()) 1 0;
		 add_resistor c 1 2 50.;
		 add_resistor c 2 0 50.;
		 add_vcvs c ~id:101 ~ref1_id:2 ~ref2_id:0 ~gain 3 0;
		 add_resistor c 3 0 50.;
              );
          in
          0.5 @=.? (r#mag_at "ac" 2 100.);
          gain *. 0.5 @=.? (r#mag_at "ac" 3 100.);
       );
     
     "vccs" >::
       (fun () ->
          let r =
            make_circuit
	      (fun c ->
		 add_voltage_source c ~id:100
		   ~ac:(AC.vac ~freq:1e3 ~mag:1. ()) 1 0;
		 add_resistor c 1 2 50.;
		 add_resistor c 2 0 50.;
		 add_voltage_source c ~id:101 3 4;
		 add_vccs c ~id:102 ~ref1_id:2 ~ref2_id:1 ~gain:10. 4 5;
		 add_resistor c 3 0 10.;
		 add_resistor c 5 0 8.;
              );
          in
          0.5 @=.? (r#real_at "ac" 2 100.);
          1. @=.? (r#real_at "ac" 1 100.);
          -.5. @=.? (r#real_at "ac" 101 100.);
          50. @=.? (r#real_at "ac" 3 100.);
          -.40. @=.? (r#real_at "ac" 5 100.);
       );
     
   "cccs" >::
      (fun () ->
	 let gain = 10. in
	 let r =
           make_circuit (fun c ->
			   add_current_source c ~id:100
			     ~ac:(AC.vac ~freq:1e3 ~mag:0.5 ()) 1 0;
			   add_resistor c 3 0 1.;
			   add_resistor c 2 0 1.;
			   add_cccs c ~ref1_id:3 ~ref2_id:1 ~id:101 ~gain 2 0;
			   
			)
	 in
	 0.5 @=.? (r#mag_at "ac" 1 100.);
	 5. @=.? (r#mag_at "ac" 2 100.);
      );

    (*"ccvs" >::
      (fun () ->
        let gain = 10. in
        let r =
          make_circuit (fun c ->
            add_voltage_source c ~id:100 ~vac:1. 1 0;
            add_resistor c 1 2 50.;
            add_resistor c 2 0 50.;
            add_ccvs c ~ref_id:100 ~id:101 ~gain 3 4;
            add_resistor c 3 0 10.;
            add_resistor c 4 0 10.;
          );
        in
          ();
          (1. /. 100.) @=.? (r#mag_at "ac" 100 100.);
          (gain *. (-1. /. 100.)) @=.?
            ((r#mag_at "ac" 3 100.) -. (r#mag_at "ac" 4 100.));
      );*)

  ]
  ];;

let _ =
  run_test_tt_main suite

