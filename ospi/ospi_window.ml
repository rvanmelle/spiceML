open GMain
open Printf
open Ocs_console
open OptParse

open GdkKeysyms
open Ospi_bindings
module K = GdkKeysyms

let p = OptParser.make
  ~usage:"%prog files"
  ~description:"A simple scheme evaluation window."
  ~version:"0.0.1" ();;
let files = OptParser.parse_argv p;;
    
let main () =
  let window = GWindow.window ~width:500 ~height:300 ~title:"OCS Console" () in
  let vbox = GPack.vbox ~packing:window#add () in

  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in
  let edit_menu = factory#add_submenu "Edit" in

  let scrollwin = GBin.scrolled_window ~packing:(vbox#pack ~expand:true) () in
  let bindings = [Bindings.init] in
  let env = Ocs_top.make_env () in
  List.iter (fun f -> f env) bindings;
  let editor = new Console.console_widget ~bindings ~files
    ~packing:scrollwin#add () in

  (* Handle keybindings *)
  let ctrl_bindings = [
    K._d,      ("Quit",  (fun () -> GMain.quit (); true), "Exit the window"); 
  ] in

  let key_press_cb ev =
    try
      let state = GdkEvent.Key.state ev in
      if List.mem `CONTROL state then begin
	let (_, cb, _) =
	  List.assoc (GdkEvent.Key.keyval ev) ctrl_bindings in
	cb ()
      end else false 
    with Not_found -> false
  in
  ignore (editor#text#event#connect#key_press ~callback:key_press_cb);
  
  ignore (window#connect#destroy ~callback:GMain.quit );
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_separator ());
  ignore (factory#add_item "Quit" ~key:_Q ~callback:window#destroy);
  let factory = new GMenu.factory edit_menu ~accel_group in
    (*ignore( factory#add_item "Copy" ~key:_C ~callback:
      (fun () -> editor#text#buffer#copy_clipboard GMain.clipboard) );*)
    ignore( factory#add_item "Cut" ~key:_X ~callback:
      (fun () -> GtkSignal.emit_unit
        editor#text#as_view GtkText.View.S.cut_clipboard) );
    ignore( factory#add_item "Paste" ~key:_V ~callback:
      (fun () -> GtkSignal.emit_unit
        editor#text#as_view GtkText.View.S.paste_clipboard) );
    window#add_accel_group accel_group;

  ignore (editor#text#event#connect#button_press
    ~callback:(fun ev ->
      let button = GdkEvent.Button.button ev in
      if button = 3 then begin
        file_menu#popup ~button ~time:(GdkEvent.Button.time ev); true
      end else false));

  window#show ();
  Random.self_init ();
  GMain.main ();;
    
let _ = main ()
