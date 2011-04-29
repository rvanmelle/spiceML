open Circuit
open Runner
open Util.MathHelpers

type lookup_fn = Gsl_vector.vector -> int -> float
type index_fn = int -> circuit_node

module type DIODE_PARAMS = sig
  val vt : float
  val io : float
end;;
  
module Diode = functor (Params:DIODE_PARAMS) -> struct
  (* This is a simple non-linear diode model:
     i = Io * (exp((vb - ve) / vt) - 1)
  *)
  (*let vt = 26e-3 and io = 1e-15*)
  open Params

  let diode_eq_f (vfn:lookup_fn) nb ne ~x =
    let vb, ve = (vfn x nb), (vfn x ne) in
    io *. ((exp ((vb -. ve) /. vt)) -. 1.)

  let diode_eq_df (vfn:lookup_fn) (ifn:index_fn) nb ne sign ~x ~y =
    let vb, ve = (vfn x nb), (vfn x ne) in
    let nb', ne' = (ifn nb), (ifn ne) in
    let sign = match sign with `POS -> 1. | `NEG -> -.1. in
    let _ = match nb' with
      | `Ground -> ()
      | `Node nb' ->
	  y.{nb'} <- y.{nb'} +. sign *. ((io /. vt) *. (exp ((vb -. ve) /. vt)))
    and _ = match ne' with
      | `Ground -> ()
      | `Node ne' ->
	  y.{ne'} <- y.{ne'} +. sign *. (-.1. *. (io /. vt) *. (exp ((vb -. ve) /. vt)))
    in
    ()

  let diode_subckt (c:circuit_driver) ~nb ~ne =
    c#bsrc nb ne ~fx:(diode_eq_f c#v nb ne)
      ~dfx:(diode_eq_df c#v c#index nb ne) ();
      
end;;

module type BJT_PARAMS = sig
  val beta : float
  val io : float
  val vt : float
end;;

module BJT = functor (Params:BJT_PARAMS) -> struct
  open Params
  (*let beta = 100. and io = 1e-15 and vt = 25e-3*)
    
  let diode_eq_f (vfn:lookup_fn) nb ne ~x =
    let vb, ve = (vfn x nb), (vfn x ne) in
    let result = io *. ((exp ((vb -. ve) /. vt)) -. 1.) in
    result

  let diode_eq_df (vfn:lookup_fn) (ifn:index_fn) nb ne sign ~x ~y =
    let vb, ve = (vfn x nb), (vfn x ne) in
    let nb', ne' = (ifn nb), (ifn ne) in
    let sign = match sign with `POS -> 1. | `NEG -> -.1. in
    let _ = match nb' with
      | `Ground -> ()
      | `Node nb' ->
	  y.{nb'} <- y.{nb'} +. sign *. ((io /. vt) *. (exp ((vb -. ve) /. vt)))
    and _ = match ne' with
      | `Ground -> ()
      | `Node ne' ->
	  y.{ne'} <- y.{ne'} +. sign *. (-.1. *. (io /. vt) *. (exp ((vb -. ve) /. vt)))
    in
    ()
      
  let isrc_eq_f (vfn:lookup_fn) nb ne nc ~x =
    let vb, ve, vc = (vfn x nb), (vfn x ne), (vfn x nc) in
    let result = beta *. io *. ((exp ((vb -. ve) /. vt)) -. (exp ((vb -. vc) /. vt))) in
    result

  let isrc_eq_df (vfn:lookup_fn) (ifn:index_fn) nb ne nc sign ~x ~y =
    let vb, ve, vc = (vfn x nb), (vfn x ne), (vfn x nc) in
    let nb', ne', nc' = (ifn nb), (ifn ne), (ifn nc) in
    let coef = (beta *. io) /. vt in
    let sign = match sign with `POS -> 1. | `NEG -> -.1. in
    let _ = match nb' with
      | `Ground -> ()
      | `Node nb' ->
	  y.{nb'} <- y.{nb'} +. sign *. (coef *. ((exp ((vb -. ve) /. vt)) -. (exp ((vb -. vc) /. vt))))
    and _ = match ne' with
      | `Ground -> ()
      | `Node ne' ->
	  y.{ne'} <- y.{ne'} +. sign *. (-.coef *. (exp ((vb -. ve) /. vt)))
    and _ = match nc' with
      | `Ground -> ()
      | `Node nc' ->
	  y.{nc'} <- y.{nc'} +. sign *. (coef *. (exp ((vb -. vc) /. vt)))
    in
    ()

  let bjt_subckt (c:circuit_driver) ~nb ~nc ~ne ~ns =
    let n1, n4 = 1001, 1004 in
    let rb = 10. and re = 14. and cap = 1e-12 in
    c#res nb n1 ~r:rb;
    c#cap n1 nc ~c:cap;
    c#cap n1 n4 ~c:cap;
    c#cap nc ns ~c:cap;
    c#res n4 ne ~r:re;
    c#bsrc n1 nc ~fx:(diode_eq_f c#v n1 nc)
      ~dfx:(diode_eq_df c#v c#index n1 nc) ();
    c#bsrc n1 n4 ~fx:(diode_eq_f c#v n1 n4)
      ~dfx:(diode_eq_df c#v c#index n1 n4) ();
    c#bsrc nc n4 ~fx:(isrc_eq_f c#v n1 n4 nc)
      ~dfx:(isrc_eq_df c#v c#index n1 n4 nc) ()

  module Test = struct
    let test_bjt_isrc () =
      let c = new_circuit () in
      c#vsrc 1 0 ~id:50 ~dc:(Source.DC.vdc 0.);
      c#res 1 0 ~r:10e3;
      c#res 2 0 ~r:10e3;
      c#vsrc 2 0 ~id:51 ~dc:(Source.DC.vdc 0.0001);
      c#bsrc 0 3 ~fx:(isrc_eq_f c#v 1 0 2)
	~dfx:(isrc_eq_df c#v c#index 1 0 2) ();
      c#vsrc 3 0 ~id:52;
      c#dc_sweep "dc_sweep" ~id:50 ~start:(0.65) ~stop:0.9 ~steps:40;
      c#plot `DC_SWEEP "dc_sweep" [52]
	
    let test_diode () =
      let c = new_circuit () in
      c#vsrc 1 0 ~id:50 ~dc:(Source.DC.vdc 0.);
      c#bsrc 1 2 ~fx:(diode_eq_f c#v 1 2) ~dfx:(diode_eq_df c#v c#index 1 2) ();
      c#res 2 0 ~r:10.;
      c#dc_sweep "dc_sweep" ~id:50 ~start:0.5 ~stop:1.5 ~steps:40;
      c#plot `DC_SWEEP "dc_sweep" [50]
  end
      
end;;
