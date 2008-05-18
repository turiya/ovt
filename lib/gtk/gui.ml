(* OCaml Visualization Toolkit (OVT)

   Copyright 2007 Cap Petschulat.

   This file is part of OVT.

   OVT is free software; you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 3 of the
   License, or (at your option) any later version.

   OVT is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General
   Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this program.  If not, see
   <http://www.gnu.org/licenses/>.  *)

let frame = ref None
let thread = ref None
let area = ref None

let visible () = !thread != None

let update f =
  frame := Some f;
  match !area with
    | None -> failwith "Ovt.Gui.update: no area active"
    | Some a -> a#misc#draw None  

let redraw window _ =
  let cr = Cairo_lablgtk.create window#misc#window in
  let { Gtk.width = width; Gtk.height = height } = window#misc#allocation in
  let fw, fh = (float_of_int width), (float_of_int height) in
    Cairo.rectangle cr 0. 0. fw fh;
    Cairo.set_source_rgb cr 1. 1. 1.;
    Cairo.fill cr;
    Cairo.set_source_rgb cr 0. 0. 0.;
    begin 
      match !frame with 
        | None -> failwith "no frame to redraw"
        | Some frame -> 
            Ovt.Render.Cairo.render_frame cr width height frame
    end;
    true

let thread_main () =
  GtkThread.main ();
  GtkThread.reset ();
  area := None;
  thread := None;
  frame := None

let show f = 
  match !thread with 
    | Some _ -> ()
    | None ->
        let window = GWindow.window ~border_width:10 () in
        let box = GPack.vbox ~packing:window#add () in
        let gtk_frame = 
          GBin.frame ~packing:(box#pack ~expand:true ~fill:true) () 
        in
        let a =
          GMisc.drawing_area ~width:256 ~height:256 ~packing:gtk_frame#add () 
        in
          ignore (window#connect#destroy GMain.quit);
          ignore (a#event#connect#expose (redraw a));
          window#show ();

          frame := Some f;
          area := Some a;
          thread := Some (Thread.create thread_main ())

let join () =
  match !thread with 
    | None -> ()
    | Some id -> Thread.join id

let _ = ignore (GtkMain.Main.init ())
