open Circuit
open Gsl_solver
open Recorder
open Engine
open Plotter
open Printf
open Runner

let pi = Util.MathHelpers.pi

let tank_ac () =
  let c = new_circuit () in
  c#vsrc 1 0 ~id:50 ~dc:0. ~ac:1.;
  c#cap 1 2 ~c:1e-6;
  c#ind 1 3 ~l:1e-4 ~id:51;
  c#res 3 2 ~r:2.;
  c#res 2 0 ~r:1e2;
  c#ac "ac0" ~f:15e3;
  c#ac_sweep "ac1" ~src:50 ~start:1000. ~stop:1e6 ~steps:50;
  printf "vac(%d) = %g\n%!" 1 (c#vac "ac0" 1 ~f:15e3);
  printf "vac(%d) = %g\n%!" 2 (c#vac "ac0" 2 ~f:15e3);
  printf "vac(%d) = %g\n%!" 3 (c#vac "ac0" 3 ~f:15e3);
  printf "vac(%d) = %g\n%!" 50 (c#vac "ac0" 50 ~f:15e3);
  printf "vac(%d) = %g\n%!" 51 (c#vac "ac0" 51 ~f:15e3);
  c#plot `AC_SWEEP "ac1" [2]

(*
  vac(1) = 1
  vac(2) = 0.702487
  vac(3) = 0.737248
  vac(50) = 0.00702487
  vac(51) = 0.0320588

  NOTE: this works... add to unit test to verity
  vac(%d) = norm (cos_term + j * sin_term ** 2)
  vphase(%d) = angle (cos_term + j * sin_term ** 2)
*)
    
let tank_hb () =
  let c = new_circuit () in
  c#vsrc 1 0 ~id:50 ~dc:0. ~ac:1.;
  c#cap 1 2 ~c:1e-6;
  c#ind 1 3 ~l:1e-4 ~id:51;
  c#res 3 2 ~r:2.;
  c#res 2 0 ~r:1e2; (* vac(2) = 0.702487 *)
  c#hb_linear ~harmonics:1 ~w:(2.*.pi*.15e3)

(* This works as well and hsould be added to a unit test *)
let tank_hb2 () =
  let c = new_circuit () in
  c#vsrc 1 0 ~id:50 ~dc:0. ~ac:1.;
  c#cap 1 2 ~c:1e-6;
  c#ind 1 3 ~l:1e-4 ~id:51;
  c#res 3 2 ~r:2.;
  c#res 2 0 ~r:1e2; (* vac(2) = 0.702487 *)
  c#hb ~harmonics:2 ~w:(2.*.pi*.15e3)

let _ = tank_hb2 ()
  

