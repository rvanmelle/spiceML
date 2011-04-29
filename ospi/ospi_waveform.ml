open Value
open Gr
open Bigarray
open Printf
open Util
open Util.ListHelpers

let to_i = int_of_float
exception ValueError of string

let karma = try Sys.getenv "KARMA_ROOT"
  with Not_found -> failwith "KARMAROOT must be set"

(* Computes tick locations using a very simple algorithm:
    - num_desired_ticks: what is the ideal number of ticks
    - fmin,fmax: minimum and maximum over the range of interest
    - log: whether to generate ticks for a log spacing
    - returns a list of floats which represents the tick locations *)
let tick_locations ?log:(log=false) num_desired_ticks fmin fmax =
  let fmin, fmax = if log then log10 fmin, log10 fmax else fmin, fmax in
  let span = fmax -. fmin in
  let mag = floor (log10 span) -. 1. in
  let scale = 10. ** mag in
  let steps = [1.; 2.; 2.5; 5.; 10.; 20.; 25.; 50.] in
  let step_unscaled =
    List.find (fun x -> (span /. (x *. scale)) <= num_desired_ticks) steps in
  let step = scale *. step_unscaled in
  let rec get_ticks cur =
    if cur > fmax then [] else cur :: (get_ticks (cur +. step))
  and start_tick = (ceil (fmin /. step)) *. step in
    if log then List.map (fun x -> 10. ** x) (get_ticks start_tick) else
      get_ticks start_tick

(* FIXME/TODO:
   - code breaks when plotting all zeros
*)

type plot_type = [`Linear | `LogX | `LogY | `LogXLogY]
type plot_stype = [`Normal | `Thumbnail]
type trace_style = [
  `Lines of line_style
| `Points of point_style
| `Impulses of point_style
| `LinesPoints of line_style * point_style]
and line_style = [`Solid | `Dashed]
and point_style = [`None | `Box | `Circle | `Diamond | `Cross]

(* This is a small data structure which holds all of the plotting
   details for a particular signal *)
type plot_signal = {
  data            : real_signal;
  trace_style     : trace_style;
  color           : Color.named_color;
  signal_name     : string option;
}

(* Convenience routine for building signals *)
let new_signal ?trace:(trace=`Lines) ?lines:(lines=`Solid) ?name
    ?points:(points=`None) ?color:(color=`AUTO) (data:Value.real_signal) =
  let trace_style = match trace with
    | `Lines -> `Lines lines
    | `Points -> `Points points
    | `Impulses -> `Impulses points
    | `LinesPoints -> `LinesPoints(lines, points)
  in
    { data = data; trace_style=trace_style; color=color; signal_name=name; }

class cairo_plot (signals:plot_signal list)
  ?thumbnail:(thumbnail=false) ?logx:(logx=false)
  ?logy:(logy=false) () =

object (self)
  (* inherit widget as super *)

  method signal_bounds () =
    Real_signal.bounds_of_list (List.map (fun s -> s.data) signals)

  method render cr (width, height) =
    Cairo.rectangle cr ~x:0. ~y:0. ~width:(float width) ~height:(float height);
    Cairo_helpers.set_color cr `TWILIGHT_BG;
    Cairo.fill cr;
    let signals = List.map (fun s ->
      {s with data = (Real_signal.sample s.data (4*width))} ) signals in
    let signal_datas = List.map (fun s -> s.data) signals in
    let g = Gr.Color.ColorGenerator.init()
    and left_margin, bottom_margin, top_margin, right_margin =
      if thumbnail then 10, 10, 10, 10 else 40, 50, 15, 15 in
    let width = width - left_margin - right_margin
    and vertical_translate = height - bottom_margin
    and horizontal_translate = left_margin
    and height = height - bottom_margin - top_margin in
    let bounds = Real_signal.bounds_of_list signal_datas in
    let real_bounds = bounds in
    let check_zero x = if x = 0. then raise (ValueError "log10 of zero") else x in
    let xval x = if logx then log10 (check_zero x) else x in
    let yval y = if logy then log10 (check_zero y) else y in
    let xloc x = (float width) *. (((xval x) -. (xval bounds.xmin)) /.
                     ((xval bounds.xmax) -. (xval bounds.xmin)))
    and yloc y = (float height) *. (((yval y) -. (yval bounds.ymin)) /.
                     ((yval bounds.ymax) -. (yval bounds.ymin))) in

      Cairo.translate cr (float horizontal_translate) (float vertical_translate) ;
      Cairo.scale cr 1. (-1.) ;
      Cairo.set_font_matrix cr (Cairo.Matrix.scale (Cairo.get_font_matrix cr) 1. (-. 1.)) ;

      (* Set up the axes and guidelines *)
      let draw_axes () =
        Cairo_helpers.set_color_rgb cr (Color.lookup `TWILIGHT_FG0) ;
        Cairo.set_line_width cr 2. ;
        Cairo.set_line_cap cr Cairo.LINE_CAP_SQUARE ;

        Cairo.move_to cr 0. 0. ;
        Cairo.line_to cr 0. (float height) ;

        Cairo.move_to cr 0. 0. ;
        Cairo.line_to cr (float width) 0. ;

        Cairo.stroke cr

      and draw_xaxis_ticks offset scale values =
        let get_pixel value =
          let pixel = (value -. offset) /. scale in
          let pixel = (floor pixel) +. 0.5 in
            pixel
        in

        let draw_tick x =
          let xpixel = get_pixel (xloc x) in
            Cairo.move_to cr xpixel (float height) ;
            Cairo.line_to cr xpixel 0.5 ;

            Cairo_helpers.set_color_rgb cr (Color.lookup `TWILIGHT_FG3) ;
            if not thumbnail then (
              let content = (sprintf "%g" x) in
                Cairo_helpers.text cr xpixel (-15.) content
                  ~valign:Cairo_helpers.Bottom ~halign:Cairo_helpers.Center ());
            Cairo_helpers.set_color_rgba cr (Color.lookup `TWILIGHT_FG0) 0.6
        in
          Cairo.set_line_width cr 1. ;
          Cairo.set_line_cap cr Cairo.LINE_CAP_SQUARE ;
          Cairo_helpers.set_color_rgba cr (Color.lookup `TWILIGHT_FG0) 0.6 ;

          List.iter draw_tick values;
          Cairo.stroke cr

      and draw_yaxis_ticks offset scale values =
        let draw_tick y =
          let ypixel = yloc y in
          let ypixel = (ypixel -. offset) /. scale in
          let ypixel = (floor ypixel) +. 0.5 in
            Cairo.move_to cr 0.5 ypixel ;
            Cairo.line_to cr (float width) ypixel ;

            Cairo_helpers.set_color_rgb cr (Color.lookup `TWILIGHT_FG3) ;
            if not thumbnail then (
              let content = (sprintf "%g" y) in
                Cairo_helpers.text cr (-5.) (ypixel -. 7.) content
                  ~valign:Cairo_helpers.Middle ~halign:Cairo_helpers.Right () );
            Cairo_helpers.set_color_rgba cr (Color.lookup `TWILIGHT_FG0) 0.6
        in
          Cairo.set_line_width cr 1. ;
          Cairo.set_line_cap cr Cairo.LINE_CAP_SQUARE ;
          Cairo_helpers.set_color_rgba cr (Color.lookup `TWILIGHT_FG0) 0.6 ;

          List.iter draw_tick values ;
          Cairo.stroke cr

      in

      let bounds = {xmax = (xloc bounds.xmax);
                    xmin = (xloc bounds.xmin);
                    ymax = (yloc bounds.ymax);
                    ymin = (yloc bounds.ymin)} in
        printf "canvas size: %d %d\n" width height;
        printf "scaled: %s\n" (string_of_signal_bounds bounds);
        printf "actual: %s\n" (string_of_signal_bounds real_bounds);
        flush stdout;
      let sig_width = bounds.xmax -. bounds.xmin
      and sig_height = bounds.ymax -. bounds.ymin in

      let xaxis_offset = bounds.xmin and
          xaxis_scale = sig_width /. (float width) and
          yaxis_offset = bounds.ymin and
          yaxis_scale = sig_height /. (float height) in

      let num_desired_xticks = max ((float width) /. 100.) 2.
      and num_desired_yticks = max ((float height) /. 75.) 2. in

      let xticks = tick_locations ~log:logx num_desired_xticks
        real_bounds.xmin real_bounds.xmax
      and yticks = tick_locations ~log:logy num_desired_yticks
        real_bounds.ymin real_bounds.ymax in
        draw_xaxis_ticks xaxis_offset xaxis_scale xticks ;
        draw_yaxis_ticks yaxis_offset yaxis_scale yticks ;

        let draw_point cr point_style x y =
          match point_style with
            | `None -> ()
            | `Box ->
                Cairo.rectangle cr ~x:(x-.3.) ~y:(y-.3.) ~width:6. ~height:6.;
                Cairo.stroke cr
            | `Circle ->
                Cairo.arc cr ~xc:x ~yc:y ~radius:3. ~angle1:0.
                  ~angle2:MathHelpers.two_pi;
                Cairo.stroke cr
            | `Diamond ->
                Cairo.move_to cr ~x:x ~y:(y-.3.);
                Cairo.line_to cr ~x:(x-.3.) ~y:y;
                Cairo.line_to cr ~x:x ~y:(y+.3.);
                Cairo.line_to cr ~x:(x+.3.) ~y:y;
                Cairo.stroke cr
            | `Cross ->
                Cairo.move_to cr ~x:(x-.4.) ~y:(y-.4.);
                Cairo.line_to cr ~x:(x+.4.) ~y:(y+.4.);
                Cairo.stroke cr;
                Cairo.move_to cr ~x:(x-.4.) ~y:(y+.4.);
                Cairo.line_to cr ~x:(x+.4.) ~y:(y-.4.);
                Cairo.stroke cr
        in

        let set_color color =
          let c,a = match color with
            | `AUTO -> (Color.ColorGenerator.next_color g), 1.0
            | _ -> (Color.lookup color), 1.0
          in
            Cairo_helpers.set_color_rgba cr c a
        in

        let draw_impulse_trace s point_style color =
          let sweep = s.real_sweep
          and trace = s.real_trace in
            for i = 0 to (Array1.dim sweep) - 1 do
              let x,y = (xloc sweep.{i}), (yloc trace.{i}) in
                Cairo.move_to cr ~x:x ~y:0.;
                Cairo.line_to cr ~x:x ~y:y;
                Cairo.stroke cr;
                draw_point cr point_style x y;
            done

	and draw_point_trace s point_style color =
          let sweep = s.real_sweep
          and trace = s.real_trace in
          for i = 0 to (Array1.dim sweep) - 1 do
            let x,y = (xloc sweep.{i}), (yloc trace.{i}) in
            draw_point cr point_style x y;
          done

        and draw_line_trace s line_style point_style color =
          let _ = match line_style with
            | `Solid -> Cairo.set_dash cr [||] 0.
            | `Dashed -> Cairo.set_dash cr [|5.; 5.;|] 0. in

          let sweep = s.real_sweep
          and trace = s.real_trace in
            (* Cairo.new_path cr; *)
            Cairo.move_to cr ~x:(xloc sweep.{0}) ~y:(yloc trace.{0});
            for i = 1 to (Array1.dim sweep) - 1 do
              let x,y = (xloc sweep.{i}), (yloc trace.{i}) in
                (*printf "%g, %g\n" x y; flush stdout;*)
                Cairo.line_to cr ~x:x ~y:y
            done ;
            Cairo.stroke cr;
            for i = 1 to (Array1.dim sweep) - 1 do
              let x,y = (xloc sweep.{i}), (yloc trace.{i}) in
                draw_point cr point_style x y
            done
        in

        let draw_trace (s,pos) =
	  set_color s.color;
	  let () = match s.signal_name with
	      Some name ->
		let x = (float width) -. 150. and y = float pos in
		Cairo.rectangle cr ~x ~y ~width:40. ~height:20.;
		Cairo.fill_preserve cr;
		Cairo_helpers.text cr (x +. 45.) y name
                  ~valign:Cairo_helpers.Middle ~halign:Cairo_helpers.Left ();
		Cairo.fill cr;
	    | None -> ()
	  in
          match s.trace_style with
            | `Lines(line_style) ->
                draw_line_trace s.data line_style `None s.color
            | `Impulses(point_style) ->
                draw_impulse_trace s.data point_style s.color
            | `Points(point_style) ->
		draw_point_trace s.data point_style s.color
            | `LinesPoints(line_style,point_style) ->
                draw_line_trace s.data line_style point_style s.color

        in
          printf "xaxis_scale=%g yaxis_scale=%g xaxis_offset=%g
              yaxis_offset=%g\n" (1. /. xaxis_scale) (1. /. yaxis_scale)
            xaxis_offset yaxis_offset; flush stdout;
          Cairo.save cr;
          Cairo.set_line_cap cr Cairo.LINE_CAP_BUTT;
          (*Cairo.set_line_width cr 0.05;*)
          Cairo.scale cr (1. /. xaxis_scale) (1. /. yaxis_scale) ;
          (* FIXME: reid... this sometimes works very poorly *)
          Cairo.set_line_width cr 1.;
          (*(2. *. (max xaxis_scale yaxis_scale));*)
          (* END FIXME *)
          Cairo.translate cr (-1. *. xaxis_offset) (-1. *. yaxis_offset) ;
	  let data = list_mapi (fun i s -> s, (100 + i*25)) signals in
          List.iter draw_trace data;

          Cairo.restore cr;
          draw_axes ();

end

let read_signal dir sweep_id trace_id =
  let cur_dir = Sys.getcwd () in
    Sys.chdir dir;
    printf "Reading marshalled header info from: %s\n%!" Simenv.header_file;
    let file_id, header_data = Qucs.read_header_info Simenv.header_file in
      printf "Reading (%s,%s)...\n" sweep_id trace_id;
      let signal = Qucs.read_signal header_data file_id
        ~sweep_id:sweep_id ~trace_id:trace_id in
      let signal = match signal with
        | Value.REAL_SIGNAL x ->
            printf "REAL_SIGNAL read: sweep=%d trace=%d\n"
              (Array1.dim x.Value.real_sweep)
              (Array1.dim x.Value.real_trace);
            x
        | Value.COMPLEX_SIGNAL x -> begin
            printf "COMPLEX_SIGNAL read: sweep=%d trace=%d\n"
              (Array1.dim x.Value.complex_sweep)
              (Array1.dim x.Value.complex_trace);
            match Functions.mag signal with
              | Value.REAL_SIGNAL x -> x
              | _ -> failwith "Unexpected result"
          end
        | _ -> failwith "Should only receive a signal"
      in
        Sys.chdir cur_dir;
        signal

let sim_data = ref []

let initialize_cairo_data (plot:cairo_plot) =
  let render (spl:cairo_plot Ospi_cairo_canvas.cairo_shit) cr (width, height) =
    plot#render cr (width, height) in
  let bounds = plot#signal_bounds () in
    {
      Ospi_cairo_canvas.xmin = bounds.xmin;
      Ospi_cairo_canvas.xmax = bounds.xmax ;
      Ospi_cairo_canvas.ymin = bounds.ymin ;
      Ospi_cairo_canvas.ymax = bounds.ymax ;
      Ospi_cairo_canvas.render_fn = render ;
      Ospi_cairo_canvas.model = plot ;
    }

(* What the hell is this?
   - because we are using ocaml user-level threads, this ensure that
   control will be passed to the ocaml interpreter on a regular basis
   from GTK
   - the selection of 100ms is fairly arbitrary- works for simulations *)
let idle_id = GMain.Timeout.add 100 (fun () -> true)

let waveform_tool packing signals =
  let vbox = GPack.vbox ~packing ()
  and memo = new GUtil.memo () in
  let thumbnailbar = GPack.hbox ~height:80 ~border_width:5 ~spacing:5
    ~packing:(vbox#pack ~expand:false) ()
  and table = GPack.table ~rows:1 ~columns:1 ~homogeneous:true
    ~packing:(vbox#pack ~expand:true) ()
  and toolbar = GButton.toolbar ~orientation:`HORIZONTAL
    ~packing:(vbox#pack ~expand:false) () in
    (* Buttons for the toolbar *)
  let bzi = GButton.tool_button ~stock:`ZOOM_IN ~label:"Zoom In" ~expand:false
    ~packing:toolbar#insert ()
  and bzo = GButton.tool_button ~stock:`ZOOM_OUT ~label:"Zoom Out" ~expand:false
    ~packing:toolbar#insert ()
  and bzf = GButton.tool_button ~stock:`ZOOM_FIT ~label:"Zoom Fit" ~expand:false
    ~packing:toolbar#insert ()
  and bsim = GButton.tool_button ~stock:`EXECUTE ~label:"Simulate" ~expand:false
    ~packing:toolbar#insert ()
  and bplot = GButton.tool_button ~stock:`PRINT_PREVIEW ~label:"Plot"
    ~expand:false ~packing:toolbar#insert ()
  and blaunch = GButton.tool_button ~stock:`EXECUTE ~label:"Launch"
    ~expand:false ~packing:toolbar#insert ()
  in

  let activate_set signals =
    List.iter (fun x -> thumbnailbar#remove x) thumbnailbar#all_children;
    let elements =
      List.map (fun signal ->
        let plot = new cairo_plot [ new_signal signal ]
          ~thumbnail:false ~logx:false ~logy:false () in
        let thumb_plot = new cairo_plot [new_signal signal]
          ~thumbnail:true ~logx:false ~logy:false () in
        let plot_data = initialize_cairo_data plot in
        let thumb_plot_data = initialize_cairo_data thumb_plot in
        let thumb = Ospi_cairo_canvas.create_thumbnail ~height:100 ~width:110
          ~packing:thumbnailbar#pack thumb_plot_data in
        let canvas = Ospi_cairo_canvas.create_canvas plot_data () in
          memo#add canvas;
          (thumb, canvas)) signals
    in
      List.iter
        (fun ((thumb:'a Ospi_cairo_canvas.thumbnail),
             (canvas:'a Ospi_cairo_canvas.canvas)) ->
          thumb#event#add [`BUTTON_PRESS];
          ignore (thumb#event#connect#button_press
                     (fun _ ->
                       (* HACK: figure out a better way to do this *)
                       if table#children != [] then
                         table#remove (List.hd table#children);
                       table#add canvas#coerce;
                       true)))

        elements;
      ()
  in
  let start_simulation () =
    toolbar#misc#set_sensitive false;
    ()

  and done_simulation new_dir = (* add_set_fn dir = *)
    sim_data := new_dir :: !sim_data;
    toolbar#misc#set_sensitive true;
    ()
  in

  let get_simulation_signals dir =
    List.map (fun (analysis_id, signal_id) ->
      read_signal dir analysis_id signal_id)
      [("acfrequency", "Gain");
       ("acfrequency", "_net3.v");
       ("acfrequency", "Out.v");
       ("acfrequency", "Phase")]
  in

    let do_zoom dir () =
      List.iter (fun w ->
                   let w = memo#find w in
                     w#zoom dir)
        table#children
    in

    let _ = bzi#connect#clicked (do_zoom `IN)
    and _ = bzo#connect#clicked (do_zoom `OUT)
    and _ = bzf#connect#clicked (do_zoom `FIT)
    in
      activate_set signals;
      let _ = bsim#connect#clicked
        (Simenv.simulate_netlist_with_callbacks
            ~src_file:(karma ^ "/data/qucs/amplifier/amplifier.txt")
            ~start_cb:start_simulation
            ~done_cb:(function
              | `Success new_dir ->
                  activate_set (get_simulation_signals new_dir);
                  done_simulation ();
              | `Failure new_dir ->
                  done_simulation ()))

      and _ = blaunch#connect#clicked Simenv.start_and_receive
      in

        (vbox :> GObj.widget)



