
let print_array a =
  Array.iteri
    (fun i v ->
      Printf.printf "%d: %e\n" i a.(i))
    a

let mk_data n =
  let a = Array.make n 0. in
  for i = n / 3 to 2 * n / 3 - 1 do
    a.(i) <- 1.
  done ;
  a

let work data =
  let n = Array.length data in
  let ws = Gsl_fft.Real.make_workspace n
  and wt = Gsl_fft.Real.make_wavetable n in
  let a = { Gsl_fft.layout = Gsl_fft.Real ;
            data = data } in
  Gsl_fft.Real.transform a wt ws ;
    print_array data;
  for i = 11 to n - 1 do
    data.(i) <- 0.
  done ;
  let wt = Gsl_fft.Halfcomplex.make_wavetable n in
  Gsl_fft.Halfcomplex.inverse a wt ws

let main =
  let data = mk_data 100 in
  print_array data ; print_newline () ;
  work data ;
  print_array data
