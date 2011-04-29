open Gsl_math
open Printf
open Gsl_helpers
open Plotter

(* Compute the gamma matrix *)
let gamma' n =
  let m = ones n n in
  for row = 0 to (n-1) do
    for col = 1 to (n/2) do
      let value = ((float (row)) *. (float (col)) *. 2. *. pi) /. (float n) in
      m.{row, col*2-1} <- (cos value);
      if (col*2) < n then m.{row, col*2} <- (sin value);
    done
  done;
  m

(* Compute the gamma matrix based on the gamma' matrix *)
let gamma2 n =
  let m = gamma' n in
  match Gsl_linalg.invert_LU (`M m) with
    | `M value -> value
    | _ -> failwith "unexpected"
    
let gamma n =
  let m = gamma' n in
  let f1 = 1. /. (float n) in
  let f2 = 2. /. (float n) in
    M.transpose_in_place m;
    for col = 0 to (n-1) do
      m.{0,col} <- m.{0,col} *. f1;
    done;
    for row = 1 to (n-1) do
      for col = 0 to (n-1) do
	let f2 = if row = n - 1 && n mod 2 = 0 then f2 /. 2. else f2 in
	m.{row,col} <- m.{row,col} *. f2
      done
    done;
    m

let idft samples =
  let n = V.length samples in
  let result = V.create n in
  let gamma' = gamma' n in
    Blas.gemv Blas.NoTrans 1. gamma' samples 0. result;
    result

let dft samples =
  let n = V.length samples in
  let result = V.create n in
  let gamma = gamma n in
    Blas.gemv Blas.NoTrans 1. gamma samples 0. result;
    result


let test () =
  let size = 5 in
  let m = gamma' size in
  let m2 = gamma size in
  let m3 = gamma2 size in
  print_matrix "gamma'" m;
  print_matrix "gamma" m2;
  print_matrix "gamma2" m3


