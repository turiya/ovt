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

open Ovt

let i = ref 0

let render width height node =
  Render.render_scene_node
    ~width ~height ~filename:(Printf.sprintf "%d.png" !i) node;
  i := !i + 1

let time f =
  let initial = Sys.time () in
    f ();
    (Sys.time ()) -. initial

let report_time f description = 
  Printf.printf "%fs %s\n%!" (time f) description

let points_100k = 
  let d = ref [] in
    for i = 0 to 100000 do
      d := ((Random.float 100.), (Random.float 100.)) :: !d
    done;
    !d

let render_points data () =
  render 100 100 
    (DataSet.scene_node (DataSet.points ~point:`Circle data))

let render_points_in_frame data () =
  render 100 100
    (Frame.scene_node_of_frame 100. 100. 
       (Frame.frame [ DataSet.points ~point:`Circle data; ]))
    
let render_path data () =
  render 100 100 
    (DataSet.scene_node (DataSet.points ~line:`Line data))

let render_path_in_frame data () =
  render 100 100
    (Frame.scene_node_of_frame 100. 100. 
       (Frame.frame [ DataSet.points ~line:`Line data; ]))

let _ =
  report_time (render_points points_100k) "render 100k points";
  report_time (render_points_in_frame points_100k) "render 100k points in frame";
  report_time (render_path points_100k) "render 100k path";
  report_time (render_path_in_frame points_100k) "render 100k path in frame";
