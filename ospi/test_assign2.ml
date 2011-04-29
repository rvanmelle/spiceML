
open OUnit
open Printf

let fcmp a b =
  assert_equal ~cmp:(OUnit.cmp_float ~epsilon:0.0001)
    ~printer:string_of_float a b

let lookup_fn vec index = vec.{index}
let index_fn node = `Node (node+1)

module BJT =
  Circuit_models.BJT(struct let beta = 100. and io = 1e-15 and vt = 25e-3 end);;
    
let suite = "assign2" >:::
  [
    "diode_eq" >::
      (fun () ->
	let x = Gsl_vector.of_array [|0.; 1.8; 1.|] in
	fcmp 0.07896296 (BJT.diode_eq_f lookup_fn 1 2 ~x)
      );

    "diode_df" >::
      (fun () ->
	let x = Gsl_vector.of_array [|0.; 1.8; 1.|] in
	let y = Gsl_vector.create ~init:0. 4 in
	BJT.diode_eq_df lookup_fn index_fn 1 2 `NEG ~x ~y;
	fcmp (-.3.15851840) y.{2};
	fcmp 3.15851840 y.{3}
      );

    "isrc_eq" >::
      (fun () ->
	let x = Gsl_vector.of_array [|0.; 1.72; 0.; 1.; 0.; 2.5|] in
	fcmp 0.32187042 (BJT.isrc_eq_f lookup_fn 1 3 5 ~x);
	let x' = Gsl_vector.of_array [|0.; 0.; 0.; 1.; 0.; -.0.72|] in
	fcmp (-.0.32187042) (BJT.isrc_eq_f lookup_fn 1 3 5 ~x:x')
      );

    "isrc_df" >::
      (fun () ->
	let x = Gsl_vector.of_array [|0.; 1.72; 0.; 1.; 0.; 2.5|] in
	let y = Gsl_vector.create ~init:0. 7 in
	BJT.isrc_eq_df lookup_fn index_fn 1 3 5 `POS ~x ~y;
	fcmp 12.8748171 y.{2};
	fcmp (-.12.874817) y.{4};
	fcmp 1.12738475e-25 y.{6};
      );
  ];;

let _ =
  run_test_tt_main suite
