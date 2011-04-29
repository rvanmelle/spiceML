open Engine
open Recorder
open Circuit
open Runner
open Printf
open Util.MathHelpers

module BJT = Circuit_models.BJT

let test_circuit_multihb mag =
  let c = new_circuit () in
  let vcc, rsig, re, rc, rl, c1, c2 =
    15., 2.5e3, 28.57e3, 20e3, 10e3, 20e-3, 20e-3 in
  let f1 = 10e3 and f2 = 10.1e3 in
  c#vsrc 10 0 ~id:50 ~ac:
    (Source.vac ~src_type:`COS ~freq:f1 ~mag ~phase:(pi /. 6.) ());
  c#vsrc 1 10 ~id:60 ~ac:
    (Source.vac ~src_type:`COS ~freq:f2 ~mag ~phase:(pi /. 6.) ());
  c#vsrc 4 0 ~id:51 ~dc:vcc;
  c#vsrc 7 0 ~id:52 ~dc:(-.vcc);
  c#res 1 2 ~r:rsig;
  c#res 3 4 ~r:rc;
  c#res 6 7 ~r:re;
  c#cap 6 0 ~c:c2;
  c#cap 3 5 ~c:c1;
  c#res 5 0 ~r:rl;
  BJT.bjt_subckt c ~nb:2 ~nc:3 ~ne:6 ~ns:0;
  c#dc "DC";
  c#multiHB "HB" ~k1:5 ~w1:f1 ~k2:2 ~w2:f2;
  c#print_table ~file:"1mv.html" "HB" [5];
  printf "|Vout(f1)|: %g\n" (c#vm "HB" 5 ~f:f1);
  printf "|Vout(f2)|: %g\n" (c#vm "HB" 5 ~f:f2);
  printf "|IM|: %g\n" (20. *. (log10 ((c#vm "HB" 5 ~f:(2. *. f1 -. f2)) /.
			 (c#vm "HB" 5 ~f:f2))))

let _ =
  Gsl_error.init () ;
  test_circuit_multihb 500e-3
