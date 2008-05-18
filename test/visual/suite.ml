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
open Visual
open Transformation
open Orientation

let render_scene_node_to_file test filename =
  Render.render_scene_node
    ~width:256
    ~height:256
    ~filename:filename
    (test ())

let render_frame_to_file test filename =
  Render.render_frame
    ~width:256
    ~height:256
    ~filename:filename
    (test ())

let tests = ref []
let register x = tests := x :: !tests

let description_1 =
  "plot [ (-1000, 0), (-500, 500), (0, 0), (250, 500), (1000, 250) ]"
let f_1 _ =
  let dataset = DataSet.points [ ((-. 1000.), 0.); ((-. 500.), 500.); 
                                 (0., 0.); (250., 500.); (1000., 250.); ] in
    Frame.frame [ dataset; ]
let _ = register
  { index = 1;
    description = description_1;
    f = render_frame_to_file f_1; }

let description_2 =
  "scatter [ (0, 0), (64, 96), (96, 32) ]"
let f_2 _ =
  let dataset = DataSet.scatter [ (0., 0.); (64., 96.); (96., 32.); ] in
    Frame.frame [ dataset; ]
let _ = register
  { index = 2;
    description = description_2;
    f = render_frame_to_file f_2; }

let description_3 = 
  "frame fit testing"
let f_3 () =
  let points = 
    Random.init 0;
    let r = ref [] in
      for i = 0 to 100 do
        r := ((Random.float 100.), (Random.float 100.)) :: !r
      done;
      !r
  in
  let data = DataSet.scatter points in
    Frame.frame 
      ~style:{ Frame.default_style with
                 Frame.scale = Frame.Fixed Transformation.Identity; }
      [ data; ]
let _ = register
  { index = 3;
    description = description_3;
    f = render_frame_to_file f_3; }

let description_4 = 
  "frame testing"
let f_4 () =
  let frame = 
    Frame.frame 
      ~style:{ Frame.default_style with Frame.
                 scale = Frame.Fixed Transformation.Identity;
                 grids = [ { Grid.
                               offsets = Interval.Periodic (0., 32.);
                               orientation = Orientation.Horizontal; };
                           { Grid.
                               offsets = Interval.Periodic (0., 32.);
                               orientation = Orientation.Vertical; }; ]; }
      []
  in
    Frame.scene_node_of_frame 256. 256. frame
let _ = register
  { index = 4;
    description = description_4;
    f = render_scene_node_to_file f_4; }

open Placement

let description_5 =
  "text placement"
let f_5 _ =
  let text = SceneGraph.text (64., 128.) "Text" in
  let above = SceneGraph.text (128., 128.) "Above" in
  let below = SceneGraph.text (128., 128.) "Below" in
  let text2 = SceneGraph.text (192., 128.) "Text" in
  let left = SceneGraph.text (128., 128.) "Left" in
  let right = SceneGraph.text (128., 128.) "Right" in
  let a = SceneGraph.place2 above CenteredH Above text in
  let b = SceneGraph.place2 below CenteredH Below text in
  let c = SceneGraph.place2 left LeftOf CenteredV text2 in
  let d = SceneGraph.place2 right RightOf CenteredV text2 in
  let circle = SceneGraph.point ~color:Color.black [ (128., 128.); ] in
  let above = SceneGraph.point ~color:Color.blue [ (128., 128.); ] in
  let below = SceneGraph.point ~color:Color.green [ (128., 128.); ] in
  let left = SceneGraph.point ~color:Color.red [ (128., 128.); ] in
  let right = SceneGraph.point ~color:Color.black [ (128., 128.); ] in
  let e = SceneGraph.place2 above CenteredH Above circle in
  let f = SceneGraph.place2 below CenteredH Below circle in
  let g = SceneGraph.place2 left LeftOf CenteredV circle in
  let h = SceneGraph.place2 right RightOf CenteredV circle in
    SceneGraph.list [ text; a; b; text2; c; d; e; f; g; h; ]
let _ = register
  { index = 5;
    description = description_5;
    f = render_scene_node_to_file f_5; }

let description_6 =
  "text orientation"
let f_6 _ =
  let a = SceneGraph.text ~orientation:Horizontal (128., 128.) "Horizontal" in
  let ab = SceneGraph.box (Util.get (SceneGraph.bound_scene_node a)) in
  let b = SceneGraph.text ~orientation:Vertical (128., 128.) "Vertical" in
  let bb = SceneGraph.box (Util.get (SceneGraph.bound_scene_node b)) in
  let p = SceneGraph.point [ (128., 128.); ] in
    SceneGraph.list [ ab; bb; a; b; p; ]
let _ = register
  { index = 6;
    description = description_6;
    f = render_scene_node_to_file f_6; }

let place_primitive a =
  let b = SceneGraph.place2 a CenteredH Below a in
  let c = SceneGraph.place2 a RightOf CenteredV b in
  let d = SceneGraph.place2 a CenteredH Below c in
  let e = SceneGraph.list [ a; b; c; d; ] in
  let f = SceneGraph.place2 e CenteredH Above e in
  let g = SceneGraph.place2 e LeftOf CenteredV f in
    SceneGraph.list [ e; f; g; ]

let description_7 =
  "placement"
let f_7 _ =
  let a = SceneGraph.point [ (128., 128.); ] in
    place_primitive a
let _ = register
  { index = 7;
    description = description_7;
    f = render_scene_node_to_file f_7; }

let description_8 =
  "placement with text; should follow same pattern as circles in 7"
let f_8 _ =
  let a = SceneGraph.text (128., 128.) "OVT" in
    place_primitive a
let _ = register
  { index = 8;
    description = description_8;
    f = render_scene_node_to_file f_8; }

let description_9 =
  "translation with text; text should line up vertically"
let f_9 _ =
  let a = SceneGraph.text (128., 128.) "OVT" in
  let b = SceneGraph.transform (Translate (0., 6.)) a in
    SceneGraph.list [ a; b; ]
let _ = register
  { index = 9;
    description = description_9;
    f = render_scene_node_to_file f_9; }

let description_10 =
  "placement with boxes; should follow same pattern as circles in 7"
let f_10 _ =
  let a = 
    SceneGraph.box (Box.bound_points (128., 128.) (144., 144.))
  in
    place_primitive a
let _ = register
  { index = 10;
    description = description_10;
    f = render_scene_node_to_file f_10; }

let description_11 =
  "some normal paths"
let f_11 _ =
  let domain = Interval.bound_points (-. 5.01) 5.01 in
  let points = Interval.generate_periodic ~origin:0. ~period:0.1 domain in
  let make_plot mean variance =
    DataSet.points 
      ~label:(Printf.sprintf "N(%f, %f)" mean variance)
      ~line:`Line
      (List.combine 
         points 
         (List.map (Util.normal_pdf ~mean ~variance) points)) in
  let datasets = List.map (make_plot 0.) (Util.float_range 1. 4.) in
    Frame.frame datasets
let _ = register
  { index = 11;
    description = description_11;
    f = render_frame_to_file f_11; }

let description_12 =
  "some normal paths with point marks"
let f_12 _ =
  let domain = Interval.bound_points (-. 5.) 5. in
  let points = Interval.generate_periodic ~origin:0. ~period:0.5 domain in
  let make_plot mean variance =
    DataSet.points
      ~point:`Automatic
      ~line:`Automatic
      ~label:(Printf.sprintf "N(%f, %f)" mean variance)
      (List.combine 
         points 
         (List.map (Util.normal_pdf ~mean ~variance) points)) in
  let datasets = List.map (make_plot 0.) (Util.float_range 1. 4.) in
    Frame.frame datasets
let _ = register
  { index = 12;
    description = description_12;
    f = render_frame_to_file f_12; }

let description_13 =
  "axis testing"
let f_13 _ =
  let left = 
    let ticks = { Axis.Ticks.default with 
                    Axis.Ticks.offsets = Interval.Count 16; } in
      { Axis.edge = Box.Left;
        Axis.ticks = [ ticks; ];
        Axis.label = Some "Left"; }
  and right =
    let ticks = { Axis.Ticks.default with 
                    Axis.Ticks.offsets = Interval.Count 4; } in
      { Axis.edge = Box.Right;
        Axis.ticks = [ ticks; ];
        Axis.label = Some "Right"; }
  and top = 
    let ticks = 
      { Axis.Ticks.default with 
          Axis.Ticks.offsets = (Interval.Fixed 
                                  [ 1.; 2.; 4.; 8.; 16.; 32.; 64.; 128.; ]); }
    in
      { Axis.edge = Box.Top;
        Axis.ticks = [ ticks; ];
        Axis.label = Some "Top"; }
  and bottom = 
    let ticks = { Axis.Ticks.default with 
                    Axis.Ticks.offsets = Interval.Periodic (100., 33.); } 
    in
      { Axis.edge = Box.Bottom;
        Axis.ticks = [ ticks; ];
        Axis.label = Some "Bottom"; }
  in
    Frame.frame
      ~style:{ Frame.default_style with Frame.
                 scale = Frame.Fixed Transformation.Identity;
                 axes = [ left; right; top; bottom; ]; }
      []
let _ = register
  { index = 13;
    description = description_13;
    f = render_frame_to_file f_13; }

let render_pdf () =
  let frame =
    let domain = Interval.bound_points (-. 5.) 5. in
    let points = Interval.generate_periodic ~origin:0. ~period:0.5 domain in
    let make_plot mean variance =
      DataSet.points
        ~point:`Automatic
        ~line:`Automatic
        ~label:(Printf.sprintf "N(%f, %f)" mean variance)
        (List.combine 
           points 
           (List.map (Util.normal_pdf ~mean ~variance) points)) in
    let datasets = List.map (make_plot 0.) (Util.float_range 1. 4.) in
      Frame.frame datasets
  in
    Render.render_frame ~width:512 ~height:512 ~filename:"test.pdf" frame

type record =
    { date: int;
      pill: float;
      prec: float;
      tmax: float;
      tmin: float;
      tavg: float;
      prcp: float; }

let record date pill prec tmax tmin tavg prcp =
  { date = date; 
    pill = pill; 
    prec = prec; 
    tmax = tmax; 
    tmin = tmin; 
    tavg = tavg; 
    prcp = prcp; }

open Scanf
let parse_banner () =
  let file = open_in "data/snotel_banner" in
    ignore (input_line file); (* header *)
    let rec iter index result =
      try 
        let line = input_line file in
          try
            iter (index + 1)
              ({ (sscanf line "%d\t%f\t%f\t%f\t%f\t%f\t%f" record) with 
                   date = index; }
               :: result)
          with Scan_failure _ -> iter index result (* bad record, try again *)
      with End_of_file -> result
    in
      iter 0 []
        
let description_14 =
  "snotel banner ridge dataset"
let f_14 _ =
  let records = parse_banner () in
  let min = 
    DataSet.plot ~label:"Minimum"
      (List.map (fun r -> r.tmin) records) 
  and avg = 
    DataSet.plot ~label:"Average"
      (List.map (fun r -> r.tavg) records) 
  and max = 
    DataSet.plot ~label:"Maximum"
      (List.map (fun r -> r.tmax) records) 
  in
    Frame.frame
      ~style:{ Frame.default_style with Frame.
                axes = [ { Axis.default_y with Axis.
                             label = Some "Degrees (C)"; };
                         { Axis.default_x with Axis.
                             label = Some "Days Since 2000-01-01"; }; ]; }
      [ min; avg; max; ]
let _ = register
  { index = 14;
    description = description_14;
    f = render_frame_to_file f_14; }



let _ = 
  Visual.run_tests !tests
