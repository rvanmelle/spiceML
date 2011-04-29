open Engine
open Recorder
open Circuit
open Runner
open Printf
open Util.MathHelpers
open Gsl_helpers.ComplexHelpers

open Value
module I = Volterra.Input
module C = Volterra.Conductance

let rads_of_degrees x = 2. *. pi *. x

(* Constants that we use through the routines here *)
let w1, w2, io, vt, rs, cpi, rload, beta, a1, a2 =
  (rads_of_degrees 10e3), (rads_of_degrees 10.1e3), 1e-15, 25e-3, 50.,
  1e-12, 10e3, 100., 1e-3, 1e-4;;

(* Construct a parameterized Diode model *)
module Diode = Circuit_models.Diode(struct let vt=vt and io=io end);;

let frange start stop iter =
  let rec generate cur =
    if cur > stop then [] else cur :: (generate (cur +. iter)) in
  let vals = Array.of_list (generate start) in
  farray_of_array vals

let plot_response () =
  let g1 = io /. vt
  and g2 = io /. (2. *. (vt ** 2.))
  and g3 = io /. (6. *. (vt ** 3.))
  in
  let z_cpi w = Complex.inv (complex 0. (w *. cpi)) in
  let z1 w =
    let term1 = Complex.inv (z_cpi w)
    and term2 = complex g1 0. 
    and term3 = complex (1. /. rs) 0.
    in
    Complex.inv (term1 $+ term2 $+ term3)
  in 
(*let num = complex ((-.1.) *. beta *. rload *. g1 *. rs) 0. in
    let num = num $* (z_cpi w) in
    let den = Complex.one $+
      ((complex g1 0.) $* ((complex rs 0.) $+ (z_cpi w))) in
    num $/ den
  in*)
  (*let z2 w = (z1 w) $/ (complex (g1 *. beta *. rload) 0.) in*)
  let z2 w = z1 w in
  let coeff = complex ((-.1.) *. beta *. rload *. g1) 0. in
  let kernel1 w = coeff $* (z1 w)
  and kernel2 w1 w2 =
    (complex (-.1. *. g2) 0.) $* coeff $* (z2 w1) $* (z2 w2) $* (z1 (w1 +. w2))
  and kernel3 w1 w2 w3 =
    (complex g3 0.) $* coeff $* (z2 w1) $* (z2 w2) $* (z2 w3) $* (z1 (w1 +. w2 +. w3))
  in
  printf "coeff=%g\n%!" (Complex.norm coeff);
  printf "z_cpi=%g\n%!" (Complex.norm (z_cpi w1));
  printf "z1=%g\n%!" (Complex.norm (z1 w1));
  printf "z2=%g\n%!" (Complex.norm (z2 w1));
  printf "kernel1=%g\n%!" (Complex.norm (kernel1 w1));
  printf "kernel2=%g\n%!" (Complex.norm (kernel2 w1 w2));
  printf "kernel3=%g\n%!" (Complex.norm (kernel3 w1 w2 w1));  
  let first_order t : Complex.t =
    let t = complex t 0. in
    let term1 k =
      let w = (float k) *. w1 in
      (complex (0.5 *. a1) 0.) $* (kernel1 w) $*
	(Complex.exp (t $* (complex 0. w)))
    and term2 k =
      let w = (float k) *. w2 in
      (complex (0.5 *. a2) 0.) $* (kernel1 w) $*
	(Complex.exp (t $* (complex 0. w)))
    in
    (term1 1) $+ (term1 (-1)) $+ (term2 1) $+ (term2 (-1))
  and second_order t =
    let t = complex t 0. in
    let term1 k1 k2 =
      let w1 = (float k1) *. w1
      and w2 = (float k2) *. w1 in
      (complex (0.25 *. (a1 ** 2.)) 0.) $* (kernel2 w1 w2) $*
	(Complex.exp (t $* (complex 0. (w1 +. w2))))
    and term2 k1 k2 =
      let w1 = (float k1) *. w2
      and w2 = (float k2) *. w2 in
      (complex (0.25 *. (a1 ** 2.)) 0.) $* (kernel2 w1 w2) $*
	(Complex.exp (t $* (complex 0. (w1 +. w2))))
    and term3 k1 k2 =
      let w1 = (float k1) *. w1
      and w2 = (float k2) *. w2 in
      (complex (0.5 *. a1 *. a2) 0.) $* (kernel2 w1 w2) $*
	(Complex.exp (t $* (complex 0. (w1 +. w2))))
    and all_terms (fn: int -> int -> Complex.t) : Complex.t =
      (fn 1 1) $+ (fn 1 (-1)) $+ (fn (-1) 1) $+ (fn (-1) (-1)) in
    (all_terms term1) $+ (all_terms term2) $+ (all_terms term3)
  and third_order t =
    let t = complex t 0. in
    let term1 k1 k2 k3 =
      let w1, w2, w3 = (float k1) *. w1, (float k2) *. w1, (float k3) *. w1 in
      (complex (0.125 *. (a1 ** 3.)) 0.) $* (kernel3 w1 w2 w3) $*
	(Complex.exp (t $* (complex 0. (w1 +. w2 +. w3))))
    and term2 k1 k2 k3 =
      let w1, w2, w3 = (float k1) *. w1, (float k2) *. w1, (float k3) *. w2 in
      (complex (3. *. 0.125 *. (a1 ** 2.) *. a2) 0.) $* (kernel3 w1 w2 w3) $*
	(Complex.exp (t $* (complex 0. (w1 +. w2 +. w3))))
    and term3 k1 k2 k3 =
      let w1, w2, w3 = (float k1) *. w1, (float k2) *. w2, (float k3) *. w2 in
      (complex (3. *. 0.125 *. (a2 ** 2.) *. a1) 0.) $* (kernel3 w1 w2 w3) $*
	(Complex.exp (t $* (complex 0. (w1 +. w2 +. w3))))
    and term4 k1 k2 k3 =
      let w1, w2, w3 = (float k1) *. w2, (float k2) *. w2, (float k3) *. w2 in
      (complex (0.125 *. (a2 ** 3.)) 0.) $* (kernel3 w1 w2 w3) $*
	(Complex.exp (t $* (complex 0. (w1 +. w2 +. w3))))
    and all_terms fn =
      (fn 1 1 1) $+ (fn 1 1 (-1)) $+ (fn 1 (-1) 1) $+ (fn 1 (-1) (-1)) $+
	(fn (-1) 1 1) $+ (fn (-1) 1 (-1)) $+ (fn (-1) (-1) 1) $+ (fn (-1) (-1) (-1))
    in
    (all_terms term1) $+ (all_terms term2) $+ (all_terms term3) $+ (all_terms term4)
  in
  let logx, logy = false, false in
  let sweep : fvector = frange 0. 0.01 0.00001 in
  let trace fn = farray_map (fun x -> Complex.norm (fn x)) sweep in
  let signal1 = {real_sweep = sweep; real_trace = trace first_order} in
  let signal2 = {real_sweep = sweep; real_trace = trace second_order} in
  let signal3 = {real_sweep = sweep; real_trace = trace third_order} in
  let wave1 = Ospi_waveform.new_signal (*~name:"1st Order Response"*)
    ~trace:`LinesPoints ~lines:`Solid ~points:`None signal1 in
  let wave2 = Ospi_waveform.new_signal (*~name:"2nd Order Response"*)
    ~trace:`LinesPoints ~lines:`Solid ~points:`None signal2 in
  let wave3 = Ospi_waveform.new_signal (*~name:"3rd Order Response"*)
    ~trace:`LinesPoints ~lines:`Solid ~points:`None signal3 in
  Plotter.plot ~logx ~logy [wave1]
  (*Plotter.plot ~logx ~logy [wave2];
  Plotter.plot ~logx ~logy [wave3]  *)
    
    
    
  
let test_circuit_volterra () =
  let c = new_circuit () in
  
  let src1 = {
    I.id  = 50;
    I.pos = c#index 1;
    I.neg = c#index 0;
    I.src = Source.AC.vac ~freq:w1 ~mag:a1 ();
  }
  and src2 = {
    I.id  = 60;
    I.pos = c#index 1;
    I.neg = c#index 0;
    I.src = Source.AC.vac ~freq:w2 ~mag:a2 ();
  }
  and conductance = {
    C.g1_fn = (fun x -> (io /. vt) *. (exp (x /. vt)));
    C.g2_fn = (fun x -> (0.5 *. io /. (vt ** 2.)) *. (exp (x /. vt)));
    C.g3_fn = (fun x -> ((1. /. 6.) *. io /. (vt ** 3.)) *. (exp (x /. vt)));
    C.pos   = c#index 1;
    C.neg   = c#index 0;
  }
  in
  c#res 1 0 ~r:rs;
  c#cap 1 0 ~c:cpi;
  c#cccs 2 0 ~id:70 ~ref1:1 ~ref2:2 ~gain:beta;
  c#res 2 0 ~r:rload;
  c#volterra "V" ~src1 ~src2 ~conductance;
  printf "v(2)=%g\n%!" (c#vdc "DC" 2)
    

let test_circuit_multihb () =
  let c = new_circuit () in
  let w1, w2, io, vt, rs, cpi, rload, beta, a1, a2 =
    2. *. pi *. 10e3, 2. *. pi *. 10.1e3, 1e-15, 25e-3, 50.,
    1e-12, 10e3, 100., 1e-3, 1e-4 in
  c#isrc 1 0 ~id:50 ~ac:(Source.AC.vac ~freq:w1 ~mag:a1 ());
  c#isrc 1 0 ~id:60 ~ac:(Source.AC.vac ~freq:w2 ~mag:a2 ());
  c#res 1 0 ~r:rs;
  c#cap 1 0 ~c:cpi;
  Diode.diode_subckt c ~nb:2 ~ne:0;
  c#cccs 2 0 ~id:70 ~ref1:1 ~ref2:2 ~gain:beta;
  c#res 2 0 ~r:rload;
  c#multiHB "HB" ~k1:5 ~w1 ~k2:2 ~w2;

  printf "|Vout(w1)|: %g\n%!" (c#vm "HB" 2 ~f:w1);
  printf "|Vout(w2)|: %g\n%!" (c#vm "HB" 2 ~f:w2);
  printf "|IM|: %g\n%!" (c#vm "HB" 2 ~f:(2. *. w1 -. w2))

let _ =
  Gsl_error.init () ;
  (*test_circuit_volterra ()*)
  plot_response ()
