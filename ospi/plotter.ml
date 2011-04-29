
module Waveform = Ospi_waveform;;

let initialize_cairo_data signals render_fn =
  let signal_datas = List.map (fun s -> s.Waveform.data) signals in
  let bounds = Real_signal.bounds_of_list signal_datas in
  {
    Ospi_cairo_canvas.xmin = bounds.Value.xmin;
    Ospi_cairo_canvas.xmax = bounds.Value.xmax ;
    Ospi_cairo_canvas.ymin = bounds.Value.ymin ;
    Ospi_cairo_canvas.ymax = bounds.Value.ymax ;
    Ospi_cairo_canvas.render_fn = render_fn ;
    Ospi_cairo_canvas.model = signals ;
  }

type cairo_data = (Waveform.plot_signal list) Ospi_cairo_canvas.cairo_shit
let render ~logx ~logy (spl:cairo_data) cr (width, height) =
  let p = new Waveform.cairo_plot spl.Ospi_cairo_canvas.model ~logx ~logy () in
    p#render cr (width, height)


(* Print the graph to the specified channel in either `SVG `PDF or PS format *)
let print ~logx ~logy signals ~outc ~format ~width ~height =
  let file_backend surface_create =
    let s = surface_create outc ~width_in_points:(float width)
      ~height_in_points:(float height) in
    let cr = Cairo.create s in
    let p = new Waveform.cairo_plot signals ~logx ~logy () in
      p#render cr (width, height);
      Cairo.show_page cr;
      Cairo.surface_finish s
  in
  let surface = match format with
      `SVG -> (Cairo_svg.surface_create_for_channel :>
                  Pervasives.out_channel -> width_in_points:float ->
                height_in_points:float -> [`Any] Cairo.surface)
    | `PDF -> (Cairo_pdf.surface_create_for_channel :>
                  Pervasives.out_channel -> width_in_points:float ->
                height_in_points:float -> [`Any] Cairo.surface)
    | `PS -> (Cairo_ps.surface_create_for_channel  :>
                 Pervasives.out_channel -> width_in_points:float ->
               height_in_points:float -> [`Any] Cairo.surface)
  in
    file_backend surface

let plot ~logx ~logy (signals:Waveform.plot_signal list) =
  let window = GWindow.window ~width:600 ~height:400
    ~title:"Plotter" () in
  let vbox = GPack.vbox ~packing:window#add () in
  let table = GPack.table ~rows:1 ~columns:1 ~homogeneous:true
    ~packing:(vbox#pack ~expand:true) () in
  let cairo_data = initialize_cairo_data signals (render ~logx ~logy) in
  let canvas = Ospi_cairo_canvas.create_canvas cairo_data () in
    table#add canvas#coerce;
    ignore (window#connect#destroy ~callback:GMain.Main.quit);
    window#show ();
    GMain.Main.main ()


          (*let hbox = GPack.hbox  ~homogeneous:false
      ~packing:(vbox#pack ~expand:true) () in*)
    (*let scrollwin = GBin.scrolled_window
      ~packing:(vbox#pack ~expand:false) ~border_width:2 ~height:125 () in*)
    (*Waveform.waveform_tool hbox#add signals;*)
  (*let pixmap = new_pixmap 400 400 in
  let cr = Cairo_lablgtk.create pixmap#pixmap in
  let cairo_plotter = new Waveform.cairo_plot signals () in
  let gwin = scrollwin#misc#window in
  let d = new GDraw.drawable gwin in
    cairo_plotter#render cr (400,400);
    d#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0 ~width:400 ~height:400
      pixmap#pixmap;*)

