open Printf
module M = Gsl_matrix
module V = Gsl_vector
module VC = Gsl_vector_complex
module C = Gsl_complex
module Blas = Gsl_blas

module ComplexHelpers = struct
  let complex re im = { Gsl_complex.re = re; Gsl_complex.im = im }
  let zero = complex 0. 0.
  let ($*) a b = Complex.mul a b
  let ($+) a b = Complex.add a b
  let ($/) a b = Complex.div a b
end;;
 
let vecs_equal (v1:V.vector) (v2:V.vector) =
  if (Gsl_vector.length v1) = (Gsl_vector.length v2) then
    let test_a = Array.mapi (fun i el -> el = v2.{i}) (V.to_array v1) in
    Array.fold_left (fun result test -> result && test) true test_a
  else false

exception Not_equal of float*float
let matrices_equal (m1:M.matrix) (m2:M.matrix) =
  if (M.dims m1) = (M.dims m2) then
    let rows, cols = M.dims m1 in
    try
      for i = 0 to rows - 1 do
	for j = 0 to cols - 1 do
	  if (Gsl_math.fcmp m1.{i,j} m2.{i,j} 1e-9) != 0 then
	    raise (Not_equal(m1.{i,j}, m2.{i,j}))
	done
      done;
      true
    with Not_equal(f1,f2) ->
      printf "%g != %g\n%!" f1 f2;
      false
  else false
  
let ($@) m (i,j) =  M.get m i j
let (=>) m (i,j) value =
  M.set m i j value
let (|*>) a x y =
  Blas.gemv Blas.NoTrans ~alpha:1. ~a:a ~x ~beta:0. ~y
let (|*|) a b c =
  Blas.gemm ~ta:Blas.NoTrans ~tb:Blas.NoTrans ~alpha:1. ~a ~b ~beta:0. ~c
let (|*~|) a b c =
  Blas.gemm ~ta:Blas.NoTrans ~tb:Blas.Trans ~alpha:1. ~a ~b ~beta:0. ~c
let (|~*|) a b c =
  Blas.gemm ~ta:Blas.Trans ~tb:Blas.NoTrans ~alpha:1. ~a ~b ~beta:0. ~c
let ones r c = M.create ~init:1. r c
let zeros r c = M.create ~init:0. r c
let zeros_v l = V.create ~init:0. l
let matrix r c = M.create r c

let blit src i0 j0 dest =
  let w,h = Gsl_matrix.dims src in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
      (*printf "blit %d,%d <- %d,%d\n%!" (i0+i) (j0+j) i j;*)
      dest.{i0+i, j0+j} <- src.{i,j}
    done
  done
  
let farray_of_vector v = Value.farray_of_array (V.to_array v)

let frange start stop samples =
  let result = Array.make samples 0. in
  let bin_size = (stop -. start) /. (float (samples - 1)) in
    for n = 0 to (samples-1) do
      result.(n) <- start +. ((float n) *. bin_size);
    done;
    result

let mathml_doc_header = "\
<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1 plus MathML 2.0 plus SVG 1.1//EN\"
    \"http://www.w3.org/2002/04/xhtml-math-svg/xhtml-math-svg.dtd\">
<?xml-stylesheet type=\"text/xsl\" href=\"ospi/etc/mathml.xsl\"?>
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">
<head><title>Assignment 1</title></head>
<body>
"

let mathml_doc_footer = "</body>\n</html>\n"

let fval value = if (abs_float value) < 1e-10 then 0. else value
let ftos value = sprintf "%g" (fval value)

type math_element = [`M of M.matrix  | `V of V.vector]
let html_of_matrix ?doc:(doc=false) (ms:(string*math_element) list) =
  let buf = Buffer.create 1024 in
  let add s = Buffer.add_string buf s in
  let do_matrix label m =
    add "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n";
    add (sprintf "<mi>%s</mi><mo>=</mo>\n" label);
    add "<matrix>\n";
    let rows, cols = M.dims m in
      for row = 0 to (rows-1) do
        add "<matrixrow>\n";
        for col = 0 to (cols-1) do
          add (sprintf "<cn>%s</cn>\n" (ftos (m $@ (row,col))));
        done;
        add "</matrixrow>\n";
      done;
      add "</matrix>\n";
      add "</math>\n";
  and do_vector label v =
    add "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">\n";
    add (sprintf "<mi>%s</mi><mo>=</mo>\n" label);
    add "<vector>\n";
    for i = 0 to (V.length v) - 1 do
      add (sprintf "<cn>%s</cn>\n" (ftos v.{i}));
    done;
    add "</vector>\n";
    add "</math>\n";
  in
  if doc then add mathml_doc_header;
  List.iter (fun (name, m) ->
    (match m with
      | `M m -> do_matrix name m
      | `V v -> do_vector name v);
    add "<br/>\n") ms;
  if doc then add mathml_doc_footer;
  Buffer.contents buf

let fhtml_of_matrix ?(file="output.xml") ms =
  let outc = open_out file in
  output_string outc (html_of_matrix ~doc:true ms);
  close_out outc

let string_of_vector label v =
  let b = Buffer.create 512 in
  Buffer.add_string b (sprintf "%s: [" label);
  for n = 0 to ((V.length v) -1) do
    Buffer.add_string b (sprintf " %g " (fval (V.get v n)));
  done;
  Buffer.add_string b (sprintf "]");
  Buffer.contents b
      
let print_vector label v = print_endline (string_of_vector label v)

let print_vector2 label v =
  for i = 0 to (V.length v) - 1 do
    printf "%s(%d) = %g\n" label i v.{i}
  done

let print_array label a =
  printf "%s: [" label;
  for n = 0 to ((Array.length a) -1) do
    printf " %g " (fval a.(n));
  done;
  printf "]\n"

let print_complex_array label a =
  printf "%s: [" label;
  for n = 0 to ((Array.length a) -1) do
    let vc = a.(n) in
    printf " %g,%g " (fval vc.Complex.re) (fval vc.Complex.im);
  done;
  printf "]\n"

let print_complex_vector label v =
  printf "%s(%d): [" label (VC.length v);
  for n = 0 to ((VC.length v) - 1) do
    let vc = v.{n} in
    printf " %g,%g " (fval vc.Complex.re) (fval vc.Complex.im);
  done;
  printf "]\n"

let print_matrix label m =
  printf "\n\n%s\n\n" label;
  let rows, cols = M.dims m in
    printf "  ";
    for col = 0 to (cols-1) do
      printf "%10d" col
    done;
    printf "\n";
    for row = 0 to (rows-1) do
      printf "%d:" row;
      for col = 0 to (cols-1) do
        let value = M.get m row col in
          printf "%10g" (fval value)
      done;
      printf "\n";
    done
