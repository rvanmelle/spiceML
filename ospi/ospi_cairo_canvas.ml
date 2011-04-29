open GObj
open GtkMisc
open Gr
open GtkBin
open Printf

type tool = {
  show     : unit -> unit;
  hide     : unit -> unit;
  select   : int -> unit;
}

type zoom_level =
  | Fit
  | Zoom of float

type zoom_direction = [ `FIT | `IN | `OUT ]

type point = Cairo.point = { x : float ; y : float }

type 'a cairo_shit = {
  xmin        : float;
  xmax        : float;
  ymin        : float;
  ymax        : float;
  render_fn             : 'a cairo_rendering_fn;
  model : 'a;
}
and 'a cairo_rendering_fn = 'a cairo_shit -> Cairo.t -> (int * int) -> unit

class virtual ['a] cairo_drawable = object
end

(* NOTE: We are currently passing in both an Fr.event and the initial
   cairo_shit data.  This is unfortunate, but requires the conversion of
   more code to make it work.  *)
class ['a] canvas obj
  (cairo_data : 'a cairo_shit) ~buffer =

  let xsize = cairo_data.xmax -. cairo_data.xmin
  and ysize = cairo_data.ymax -. cairo_data.ymin
  in

object (self)
  inherit GBin.scrolled_window (obj : Gtk.scrolled_window Gtk.obj)
  inherit ['a] cairo_drawable

  val zoom = new GUtil.variable Fit
  val viewport = GBin.viewport ()
  val da = GMisc.drawing_area ()
  val mutable pixmap = GDraw.pixmap ~width:1 ~height:1 ()

  method drawing_area_bounds =
    match zoom#get with
      | Fit ->
          let window = viewport#misc#window in
            Gdk.Drawable.get_size window
      | Zoom z ->
          let xpixels = int_of_float (xsize *. z) and
              ypixels = int_of_float (ysize *. z) in
            xpixels, ypixels

  (* This should be called when the model has changed and a redraw is
     required *)
  method private forced_redraw cairo_data =
    let da_width, da_height = self#drawing_area_bounds in
    let win = da#misc#window in
    let context = Cairo_lablgtk.create pixmap#pixmap in
    let drawable = new GDraw.drawable win in
      cairo_data.render_fn cairo_data context (da_width, da_height);
      drawable#put_pixmap ~x:0 ~y:0 ~xsrc:0 ~ysrc:0
        ~width:da_width ~height:da_height pixmap#pixmap

  method zoom_factor : float =
    (* Always returns a float; this is not the zoom_level, but the zoom factor *)
    match zoom#get with
      | Zoom z -> z
      | Fit ->
          let window = viewport#misc#window in
          let width, height = Gdk.Drawable.get_size window in
            min ((float width) /. xsize) ((float height) /. ysize)

  method zoom (dir:zoom_direction) =
    let zoom_amount = 1.25 in
      match dir with
        | `OUT -> zoom#set (Zoom (self#zoom_factor /. zoom_amount))
        | `IN -> zoom#set (Zoom (self#zoom_factor *. zoom_amount))
        | `FIT -> zoom#set Fit

  (*method motion_e () =
    da#event#add [`BUTTON_MOTION];
    lift_event da true da#event#connect#motion_notify*)

  initializer
  let _ = zoom#connect#changed
    (fun z ->
      let width, height = match z with
        | Fit -> 1, 1
        | Zoom z -> self#drawing_area_bounds
      in
        da#set_size ~width ~height)
  in
    viewport#add (da :> GObj.widget);
    self#add (viewport :> GObj.widget);

    (* IMPORTANT!  Notice that the events below are not yet integrated
    into the FR event framework.  This means that they will be using
    the wrong data if forced to update after a model_changed_e event.
    It is possible to hook them up as was done in the thumbnail class below,
    however, I'm hoping that I can come up with a better/cleaner methodology
    before tackling this.

    Help wanted! *)
    (*da#event#add [ `KEY_PRESS ;
                   `BUTTON_MOTION ;
                   `BUTTON_PRESS ;
                   `BUTTON_RELEASE ] ;*)
    if buffer then
      ignore (da#event#connect#expose (fun ev ->
        let area = GdkEvent.Expose.area ev in
        let width = Gdk.Rectangle.width area
        and height = Gdk.Rectangle.height area
        and x = Gdk.Rectangle.x area
        and y = Gdk.Rectangle.y area in
	
        if width != 0 && height != 0 then begin
          let pm_width, pm_height = pixmap#size in
          let da_width, da_height = self#drawing_area_bounds in
          if pm_width != da_width || pm_height != da_height then
            begin
              pixmap <- GDraw.pixmap ~width:da_width ~height:da_height ();
              let context = Cairo_lablgtk.create pixmap#pixmap in
              cairo_data.render_fn cairo_data context (da_width, da_height)
            end;
	  
          let win = da#misc#window in
          let drawable = new GDraw.drawable win in
          drawable#put_pixmap ~x ~y ~xsrc:x ~ysrc:y
            ~width:width ~height:height pixmap#pixmap ;
          true
        end else true))
    else
      ignore (da#event#connect#expose
                 (fun ev ->
                   let window = da#misc#window in
                   let cr = Cairo_lablgtk.create window in
                   let width, height = self#drawing_area_bounds in
                     cairo_data.render_fn cairo_data cr (width, height);
                     true));
    self#set_shadow_type `NONE;
    viewport#set_shadow_type `NONE;
    self#set_hpolicy `AUTOMATIC;
    self#set_vpolicy `AUTOMATIC

end

let create_canvas model ?(buffer=true) =
  ScrolledWindow.make_params []
    ~cont:(GContainer.pack_container
             ~create:(fun pl ->
               new canvas (ScrolledWindow.create pl) model ~buffer))


class ['a] thumbnail ~height ?width obj
  (cairo_data : 'a cairo_shit) =

  let desired_width = width in
  let width = Option.default 1 desired_width in

object (self)

  inherit GMisc.drawing_area (obj : Gtk.drawing_area Gtk.obj)
  inherit ['a] cairo_drawable

  val mutable pixmap = GDraw.pixmap ~width ~height ()

  (* This should be called when the model has changed and a redraw is
     required *)
  method private forced_redraw cairo_data =
    failwith "forced_redraw for thumbnails is not implemented yet"

  initializer
    begin
      ignore (self#event#connect#configure (fun ev ->
        let width = GdkEvent.Configure.width ev
        and height = GdkEvent.Configure.height ev in
        Printf.printf "Configure to size %d %d\n%!" width height;
        if (width != 0) && (height != 0) then begin
          let _ = match desired_width with
            | None ->
                let xsize = cairo_data.xmax -. cairo_data.xmin and
                    ysize = cairo_data.ymax -. cairo_data.ymin in
                let yscale = (float height) /. ysize in
                let width = int_of_float (xsize *. yscale) in
                self#misc#set_size_request ~width ~height ()
            | Some w ->
                self#misc#set_size_request ~width:w ~height ()
          in
          pixmap <- GDraw.pixmap ~width ~height ();
          let context = Cairo_lablgtk.create pixmap#pixmap in
          cairo_data.render_fn cairo_data context (width, height);
          true
        end else true));

      ignore (self#event#connect#expose (fun ev ->
        let area = GdkEvent.Expose.area ev in
        let width = Gdk.Rectangle.width area
        and height = Gdk.Rectangle.height area
        and x = Gdk.Rectangle.x area
        and y = Gdk.Rectangle.y area in
        let win = self#misc#window in
        let drawable = new GDraw.drawable win in
        drawable#put_pixmap ~x ~y ~xsrc:x ~ysrc:y
          ~width:width ~height:height pixmap#pixmap ;
        true));
    end
      
end


let create_thumbnail ~height ?width ?packing ?show model =
  let w = DrawingArea.create [] in
    pack_return (new thumbnail ~height ?width w model) ~packing ~show


(* Code for creating our nice widget with thumbnails, inspector, drawing
   canvas, and toolbar *)

let make_fancy_tool ?packing ?buttons (models:'a list) init_fn render_fn =
  let memo = new GUtil.memo () in
  let tool = GPack.hbox ?packing () in
  let main_area = GPack.vbox ~packing:tool#add ()
      (* this is the inspector, not assigned because it is currently unused *)
  and _ = GText.view ~width:150 ~packing:(tool#pack ~expand:false) () in

  (* Components of the main area *)
  let thumbnailbar = GPack.hbox ~height:80 ~border_width:5 ~spacing:5
    ~packing:(main_area#pack ~expand:false) ()

  and placeholder = GPack.table ~homogeneous:true ~rows:1 ~columns:1
    ~packing:main_area#add () in

  let toolbar = GButton.toolbar ~orientation:`HORIZONTAL
    ~packing:(main_area#pack ~expand:false) ()
  in

    (* Buttons for the toolbar *)
  let b1 = GButton.tool_button ~stock:`ZOOM_IN ~label:"Zoom In" ~expand:false
    ~packing:toolbar#insert ()
  and b2 = GButton.tool_button ~stock:`ZOOM_OUT ~label:"Zoom Out" ~expand:false
    ~packing:toolbar#insert ()
  and b3 = GButton.tool_button ~stock:`ZOOM_FIT ~label:"Zoom Fit" ~expand:false
    ~packing:toolbar#insert ()
  in

    (* Any extra buttons for the toolbar *)
  let _ = match buttons with
    | Some tb ->
        List.iter (fun ((button:(?use_underline:bool ->
                                 ?homogeneous:bool ->
                                 ?expand:bool ->
                                 ?packing:(GButton.tool_item_o -> unit) ->
                                 ?show:bool -> unit -> GButton.tool_button)),
                         callback) ->
                     let b = button ~expand:false ~packing:toolbar#insert () in
                       b#connect#clicked callback;
                       ())
          tb
    | None -> ()
  in

  let thumbnail_pack_fn = (fun widget -> thumbnailbar#pack widget) in
  let elements =
    List.map (fun t ->
      let cairo_data = init_fn t render_fn in
      let thumb = create_thumbnail ~height:100
        ~packing:thumbnail_pack_fn cairo_data in
      let canvas = create_canvas cairo_data () in
        memo#add canvas;
        (thumb, canvas))
      models
  in
  let elements = List.map
    (fun (thumb, canvas) -> thumb, (canvas :> GObj.widget)) elements in

    List.iter
      (fun ((thumb:'a thumbnail), (canvas:GObj.widget)) ->
         thumb#event#add [`BUTTON_PRESS];
         ignore (thumb#event#connect#button_press
                   (fun _ ->
                      let children = placeholder#children in
                      let oid = canvas#get_oid in
                      let child_oids = List.map (fun c -> c#get_oid) children in
                        if not (List.mem oid child_oids) then
                          begin
                            placeholder#add canvas;
                            List.iter (fun c -> placeholder#remove c) children
                          end;
                        flush stdout;

                      true)))

      elements ;

    (* Callbacks *)
    let do_zoom dir () =
      List.iter (fun w ->
                   let w = memo#find w in
                     w#zoom dir)
        placeholder#children
    in

    let _ = b1#connect#clicked (do_zoom `IN)
    and _ = b2#connect#clicked (do_zoom `OUT)
    and _ = b3#connect#clicked (do_zoom `FIT)
    in

    (tool :> GObj.widget)



