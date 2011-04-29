open OUnit
open Printf
open Util.TestHelpers
open Gsl_helpers

let gamma5 = [|
  [| 1.; 1.; 0.; 1.; 0.; |];
  [| 1.; 3.09016994e-1; 9.510565163e-1; -.8.09016994e-1; 5.87785252e-1; |];
  [| 1.; -.8.09016994e-1; 5.87785253e-1; 3.0901699e-1; -.9.51056516e-1; |];
  [| 1.; -.8.0901699e-1; -.5.8778525e-1; 3.09016994e-1; 9.51056516e-1; |];
  [| 1.; 3.0901699e-1; -.9.51056516e-1; -.8.0901699e-1; -.5.87785252e-1; |];
|]
  

let suite = "dsp" >::: [

  "compare gamma to gsl" >::
    (fun () ->
      printf "compare gamma to gsl\n%!";
      let size = 9 in
      let v = V.create size
      and v2 = V.create size
      and gamma = Dft.gamma size in
      (* Generate random array *)
      for i = 0 to size -1 do
	v.{i} <- Random.float 10.;
	printf "%g\n" v.{i};
      done;
      (* Get my answer *)
      (*let v2 = Dft.dft v in*)
      (gamma |*> v) v2;
      (* get the gsl answer *)
      let data = V.to_array v in
      let ws = Gsl_fft.Real.make_workspace size
      and wt = Gsl_fft.Real.make_wavetable size in
      let a = { Gsl_fft.layout = Gsl_fft.Real; data = data } in
      Gsl_fft.Real.transform a wt ws ;
      (* compare answers *)
      for i = 0 to size - 1 do
	printf "data.(i) = %g   v.{i} = %g\n%!" data.(i) v2.{i};
	(*v2.{i} @=.? data.(i)*)
      done;
    );
  
  "compare gamma' to gsl" >::
    (fun () ->
      printf "examine gamma'\n%!";
      let size = 9 in
      let v = V.create size
      and v2 = V.create size
      and gamma' = Dft.gamma' size in
      (* Generate random array *)
      for i = 0 to size -1 do
	v.{i} <- Random.float 10.;
	printf "%g\n" v.{i};
      done;
      (* Get my answer *)
      (gamma' |*> v) v2;
      (* get the gsl answer *)
      let data = V.to_array v in
      let a = { Gsl_fft.layout = Gsl_fft.Halfcomplex; data = data } in
      let ws = Gsl_fft.Real.make_workspace size
      and wt = Gsl_fft.Halfcomplex.make_wavetable size in
      Gsl_fft.Halfcomplex.inverse a wt ws;
      (* compare answers *)
      for i = 0 to size - 1 do
	printf "data.(i) = %g   v.{i} = %g\n%!" data.(i) v2.{i};
	(*v2.{i} @=.? data.(i)*)
      done;
    );

  "test_gsl" >::
    (fun () ->
      let size = 9 in
      let v = V.create size in
      (* Generate random array *)
      for i = 0 to size -1 do
	v.{i} <- Random.float 10.
      done;
      let copy = V.copy v in
      let data = V.to_array copy in
      let ws = Gsl_fft.Real.make_workspace size
      and wt = Gsl_fft.Real.make_wavetable size in
      let a = { Gsl_fft.layout = Gsl_fft.Real; data = data } in
      Gsl_fft.Real.transform a wt ws ;
      
      let wt = Gsl_fft.Halfcomplex.make_wavetable size in
      Gsl_fft.Halfcomplex.inverse a wt ws;
      for i = 0 to size - 1 do
	v.{i} @=.? data.(i)
      done;
    );
  
  "back and forth vector" >::
    (fun () ->
      let size = 7 in
      let v = V.create size
      and v2 = V.create size
      and v3 = V.create size in
      let gamma = Dft.gamma size
      and gamma' = Dft.gamma' size in
      for i = 0 to size -1 do
	v.{i} <- Random.float 10.
      done;
      (gamma' |*> v) v2;
      (gamma |*> v2) v3;
      for i = 0 to size - 1 do
	v.{i} @=.? v3.{i}
      done;

      (gamma |*> v) v2;
      (gamma' |*> v2) v3;
      for i = 0 to size - 1 do
	v.{i} @=.? v3.{i}
      done;
    );
      
  "back and forth matrix" >::
    (fun () ->
      let size = 7 in
      let m = M.create size size in
      let m2 = M.create size size in
      let m3 = M.create size size in
      let gamma = Dft.gamma size
      and gamma' = Dft.gamma' size in
      for i = 0 to size - 1 do
	for j = 0 to size - 1 do
	  m.{i,j} <- Random.float 10.
	done;
      done;
      (gamma |*| m) m2;
      (gamma' |*| m2) m3;
      for i = 0 to size - 1 do
	for j = 0 to size - 1 do
	  m.{i,j} @=.? m3.{i,j}
	done;
      done;
    );
];;

let _ =
  run_test_tt_main suite
