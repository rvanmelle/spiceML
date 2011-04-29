open OUnit
open Gsl_helpers
open Printf
open Engine
open Util.TestHelpers
module HB = GslEngine.HB
module S = GslEngine.Solver

let suite = "hb" >::: [

  "permutation1" >::
    ( fun () ->
      let s = S.create 2 in
      let hb = HB.create_single_tone s ~xinit:(Array.make 2 0.) ~size:3 ~w:5. in
      let v_h = V.of_array (Array.init 14 (fun x -> float (x+1))) in
      let v_n = HB.harmonic_to_node hb v_h in
      let expected_v = [|1.; 3.; 5.; 7.; 9.; 11.; 13.; 2.; 4.; 6.;
			 8.; 10.; 12.; 14.;|] in
      assert_equal ~cmp:vecs_equal ~printer:(string_of_vector "foo")
	(V.of_array expected_v) v_n;
      let new_v_h = HB.node_to_harmonic hb v_n in
      assert_equal ~cmp:vecs_equal ~printer:(string_of_vector "foo2")
	v_h new_v_h;
      (* Could also test that P' * P = Identity *)
    );

  "permutation2" >::
    ( fun () ->
      let s = S.create 3 in
      let hb = HB.create_single_tone s ~xinit:(Array.make 3 0.) ~size:2 ~w:5. in
      let v_h = V.of_array (Array.init 15 (fun x -> float (x+1))) in
      let v_n = HB.harmonic_to_node hb v_h in
      let expected_v = [|1.; 4.; 7.; 10.; 13.; 2.; 5.; 8.; 11.; 14.;
			 3.; 6.; 9.; 12.; 15.;|] in
      assert_equal ~cmp:vecs_equal ~printer:(string_of_vector "foo")
	(V.of_array expected_v) v_n;
      let new_v_h = HB.node_to_harmonic hb v_n in
      assert_equal ~cmp:vecs_equal ~printer:(string_of_vector "foo2")
	v_h new_v_h;
      (* Could also test that P' * P = Identity *)
    );

  "construction of Y_bar" >::
    (fun () ->
      let s = S.create 2 in
      let harmonics = 3 and w = 5. in
      S.set_a_matrix s 0 0 5.;
      S.set_a_matrix s 1 1 8.;
      S.set_c_matrix s 0 1 1.;
      S.set_c_matrix s 1 0 2.;
      let hb = HB.create_single_tone s ~xinit:(Array.make 2 0.)
	~size:harmonics ~w in
      HB.print hb;
      let m = hb.HB.y_bar in
      let w,h = M.dims m in
      w @=? 14; h @=? 14;
      
      (* First G matrix *)
      (m $@ (0,0)) @=.? 5.;
      (m $@ (0,1)) @=.? 0.;
      (m $@ (1,0)) @=.? 0.;
      (m $@ (1,1)) @=.? 8.;

      (* Second G matrix *)
      (m $@ (2,2)) @=.? 5.;
      (m $@ (2,3)) @=.? 0.;
      (m $@ (3,2)) @=.? 0.;
      (m $@ (3,3)) @=.? 8.;

      (* First pair of C matrices for wo... *)
      (m $@ (2,5)) @=.? 5.;
      (m $@ (3,4)) @=.? 10.;
      (m $@ (2,4)) @=.? 0.;
      (m $@ (3,5)) @=.? 0.;
      (m $@ (4,2)) @=.? 0.;
      (m $@ (4,3)) @=.? -.5.;
      (m $@ (5,2)) @=.? -.10.;
      (m $@ (5,3)) @=.? 0.;
    );

  "dfdx application of gamma/gamma'" >::
    (fun () ->
      let count = 5 and size = 4 in
      let t = { HB.circuit_size   = size;
		HB.sample_size    = count;
		HB.harmonic_map   = [];
		HB.total_size     = size * count;
		HB.permut_m       = M.create 1 1;
		HB.permut_m'      = M.create 1 1;
		HB.idft_m         = M.create 1 1;
		HB.dft_m          = M.create 1 1;
		HB.y_bar          = M.create 1 1;
		HB.b_bar          = V.create 1;
		HB.x_bar          = V.create 1;
		HB.x_init         = V.create 1;
      } in
      let test_m = M.create t.HB.total_size t.HB.total_size in
      for i = 0 to size - 1 do
	for j = 0 to size - 1 do
	  for l = 0 to count - 1 do
	    for m = 0 to count - 1 do
	      test_m.{i*count+l,j*count+m} <- (float i) +. (float j) +. 1.;
	    done;
	  done
	done;
      done;
      let result_m = HB.apply_gamma_gamma' t test_m in
      

      let expected_m = M.create t.HB.total_size t.HB.total_size in
      let gamma = Dft.gamma count and gamma' = Dft.gamma' count in
      let temp1_m = M.create count count and temp2_m = M.create count count in
      for i = 0 to size - 1 do
	for j = 0 to size - 1 do
	  M.set_all temp1_m ((float i) +. (float j) +. 1.);
	  (gamma |*| temp1_m) temp2_m;
	  (temp2_m |*| gamma') temp1_m;
	  blit temp1_m (i*count) (j*count) expected_m
	done
      done;
      assert_equal ~cmp:matrices_equal result_m expected_m;
    );
];;

let _ =
  run_test_tt_main suite
  
