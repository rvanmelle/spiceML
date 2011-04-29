open Gsl_helpers
open Printf
open Dft
open ExtString

let abs f =
  match Functions.abs (Value.FVECTOR f) with
    | Value.FVECTOR r -> r
    | _ -> failwith "wtf?"

let test_func_sin n =
  let xvals = frange 0. (8. *. Gsl_math.pi) n in
  let yvals = Array.make n 0. in
    for i = 0 to (n-1) do
      Array.set yvals i (sin (Array.get xvals i))
    done;
    xvals, yvals

let test_func_triangle n =
  let xvals = frange 0. 2. n in
  let yvals = Array.make n 0. in
  let test_func = Gsl_interp.make_interp Gsl_interp.LINEAR
    [| 0.; 0.5; 1.; 1.5; 2.|] [| 1.; 2.; 1.; 0.; 1. |] in
    Gsl_interp.eval_array test_func xvals yvals;
    xvals, yvals

let test_idft outc =
  let size = 20 in
  let xvals, yvals = test_func_triangle size in
  let dft_result = dft (V.of_array yvals) in
  let idft_result = idft dft_result in
  let sig1 = {Value.real_sweep = Value.farray_of_array xvals;
              Value.real_trace = Value.farray_of_array yvals; }
  and sig2 = {Value.real_sweep = Value.farray_of_array xvals;
              Value.real_trace = farray_of_vector idft_result } in
  let plot1 = Waveform.new_signal ~trace:`LinesPoints ~color:`RED
    ~lines:`Dashed sig1 ~points:`Cross
  and plot2 = Waveform.new_signal ~color:`YELLOW sig1 in
    match `Plot with
      | `Print ->
          Plotter.print [plot2; plot1] ~outc:outc ~format:`PDF
            ~logx:false ~logy:false ~width:400 ~height:400
      | `Plot ->
          Plotter.plot ~logx:false ~logy:false [plot2; plot1]

let test_gsl_fft () =
  let xvals, yvals = test_func_triangle 100 in
    print_array "y" yvals;
    let result = gsl_fft yvals in
    print_array "result" result

let test_dft outc =
  let size = 99 in
  let xvals, yvals = test_func_triangle size in
  let result = dft (V.of_array yvals) in
    V.add_constant result 1e-16;
    (*V.scale result ((float size) /. 2.);*)
    print_array "x" xvals;
    print_array "y" yvals;
    print_vector "result" result;
    let xvals = Value.farray_of_array (Array.init (size/2 - 1) (fun x -> float x)) in
    let real_sig v =
      let r = V.create ~init:0. (size/2-1) in
        for i = 1 to (size/2 - 1) do
          V.set r (i-1) (V.get v (i*2 - 1));
        done;
        { Value.real_sweep = xvals;
          Value.real_trace = abs (farray_of_vector r); }
    and imag_sig v =
      let r = V.create ~init:0. (size/2-1) in
        for i = 1 to (size/2 - 1) do
          V.set r (i-1) (V.get v (i*2));
        done;
        { Value.real_sweep = xvals;
          Value.real_trace = abs (farray_of_vector r); }
    in
    let sr1 = real_sig result
    and si1 = imag_sig result
    and sr2 = {Value.real_sweep = xvals;
               Value.real_trace = abs (Value.farray_of_array Octave.xr100); }
    and si2 = {Value.real_sweep = xvals;
               Value.real_trace = abs (Value.farray_of_array Octave.xi100); }
    in
    let pr1 = Waveform.new_signal ~trace:`Impulses sr1 ~points:`Cross
    and pi1 = Waveform.new_signal ~trace:`Impulses si1 ~points:`Circle
    and pr2 = Waveform.new_signal ~trace:`LinesPoints sr2 ~points:`Diamond
    and pi2 = Waveform.new_signal ~trace:`LinesPoints si2 ~points:`Box
    in
      match `Print with
        | `Print ->
            Plotter.print [pr1; pi1; pr2; pi2] ~outc:outc ~format:`PDF
              ~logx:false ~logy:true ~width:400 ~height:400
        | `Plot ->
            Plotter.plot ~logx:false ~logy:true [pr1; pi1; pr2; pi2]

let test_print outc =
  let n = 9 in
  let m = gamma' n in
  let m2 = gamma n in
  let m3 = zeros n n in
  let result = zeros n n in
  let result2, result3 = zeros n n, zeros n n in
    print_matrix "gamma inverse" m;
    print_matrix "gamma" m2;
    M.transpose m3 m;
    print_matrix "gamma_inverse Transpose" m3;
    (*Gsl_vectmat.m_mul (`M m3) (`M m);*)
    (m3 |*| m) result;
    (m2 |*| m) result2;
    (m |*| m2) result3;
    (*print_matrix "gamma_inverse T * gamma_inverse" m3;*)
    print_matrix "round 2" result;
    output_string outc (html_of_matrix [
      ("gamma inverse T x gamma inverse", result);
      ("gamma inverse T", m3); ("gamma inverse", m); ("gamma", m2);
      ("gamma inverse x gamma", result3);
      ("gamma x gamma inverse", result2)])

let remove_extraneous_xml_decls inf outf =
  let inc = open_in inf in
  let outc = open_out outf in
  let first = ref true in
    try while true do
      let line = (input_line inc) in
        if not (not !first && (String.starts_with line "<?xml version=")) then
          (output_string outc line;
           output_string outc "\n");
        first := false
      done
    with _ ->
      close_in inc;
      close_out outc

let assign1_part3 outc =
  let points = 400/5 in
  let x, y = (Array.make points 0.), (Array.make points 0.) in
    for i = 0 to (points-1) do
      let size = (i+1) * 5 in
      let xvals, yvals = test_func_triangle size in
      let result = dft (V.of_array yvals) in
        (*V.scale result ((float size) /. 2.);*)
        V.add_constant result 1e-16;
        Array.set x i (float size);
        match `Lowest with
          | `Lowest ->
              Array.set y i (abs_float (V.get result 1));
          | `Highest ->
              Array.set y i (abs_float (V.get result (size-1)));

    done;
    let signal_data = {Value.real_sweep = Value.farray_of_array x;
                       Value.real_trace = Value.farray_of_array y; } in
    let plot_data = Waveform.new_signal ~trace:`Impulses signal_data in
      match `Print with
        | `Print ->
            Plotter.print [plot_data] ~outc:outc ~format:`PDF
              ~logx:false ~logy:true ~width:400 ~height:400
        | `Plot ->
            Plotter.plot ~logx:false ~logy:true [plot_data]

let assign1 () =
  let outc = open_out "assign1.tmp" in
    output_string outc mathml_doc_header;
    test_dft outc;
    test_print outc;
    output_string outc mathml_doc_footer;
    close_out outc;
    remove_extraneous_xml_decls "assign1.tmp" "assign1.xml"

let _ =
  match `Case2 with
    | `Case1 ->
        let outc = open_out "assign1.xml" in
          output_string outc mathml_doc_header;
          test_print outc;
          output_string outc mathml_doc_footer
    | `Case2 ->
        test_dft (open_out "/tmp/baby.pdf")
    | `Case3 ->
        assign1_part3 (open_out "/tmp/baby.pdf")
    | `Case4 ->
        test_idft (open_out "/tmp/baby.pdf")
    | `Case5 ->
        test_gsl_fft ()

