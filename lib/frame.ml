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

open Orientation
open Placement
open Transformation

type scale =
    | Fixed of Transformation.t
    | Fit

type style =
    { scale: scale;
      axes: Axis.t list;
      grids: Grid.t list; 
      legend: bool; }

let default_style =
  { scale = Fit;
    axes = [ Axis.default_x; Axis.default_y; ];
    grids = [];
    legend = true; }

type content = DataSet.t list
type t = style * content

let scene_node_of_axis width height transformation axis =
  Axis.scene_node 
    axis 
    (Box.bound_points (0., 0.) (width, height)) 
    transformation

let scene_node_of_grid width height transformation grid =
  let box = 
    Transformation.transform_box 
      (Transformation.invert transformation)
      (Box.bound_points (0., 0.) (width, height))
  in
    SceneGraph.transform 
      transformation
      (Grid.scene_node grid box)

let scene_node_of_legend width height contents =
  SceneGraph.place2 
    (Legend.scene_node 
       (List.map DataSet.legend_entry contents))
    ~horizontal:AlignRight 
    ~vertical:AlignTop 
    ~anchor:(SceneGraph.box (Box.bound_points (0., 0.) (width, height)))

let rec generate count source =
  let rec loop i =
    if i >= count
    then []
    else (List.nth source (i mod (List.length source))) :: (loop (i + 1))
  in
    loop 0

let order_colors used =
  let all = [ Color.blue; Color.green; Color.red; ] in
  let a, b = List.partition (fun c -> not (List.mem c used)) all in
    a @ b

let color_contents contents =
  let attributes = Util.strip_none (List.map DataSet.color contents) in
  let automatic, assigned = List.partition Attribute.is_automatic attributes in
  let used = List.map Attribute.get_automatic assigned in
  let coloring = order_colors used in
  let colors = generate (List.length automatic) coloring in
    List.iter2 Attribute.set_automatic automatic colors    

let order_shapes used =
  let all = [ `Circle; `Square; `Diamond; ] in
  let a, b = List.partition (fun c -> not (List.mem c used)) all in
    a @ b

let shape_contents contents =
  let attributes = Util.strip_none (List.map DataSet.shape contents) in
  let automatic, assigned = List.partition Attribute.is_automatic attributes in
  let used = List.map Attribute.get_automatic assigned in
  let shaping = order_shapes used in
  let shapes = generate (List.length automatic) shaping in
    List.iter2 Attribute.set_automatic automatic shapes    

let frame ?(style=default_style) content = (style, content)

let scene_node_of_frame ~width ~height (style, content) =
  color_contents content;
  shape_contents content;
  let t = match style.scale with
    | Fixed t -> t
    | Fit -> 
        match
          Box.bound_box_list 
            (Util.strip_none 
               (List.map DataSet.bound content))
        with
          | None -> Transformation.Identity
          | Some bound ->
              let sx = width /. (Box.width bound)
              and sy = height /. (Box.height bound) in
              let t = Point.emul ((-. 1.), (-. 1.)) (Box.lower_left bound) in
                Compose ((Scale (sx, sy)), (Translate t)) in
  let content_nodes =
    [ SceneGraph.transform t
        (SceneGraph.list (List.map DataSet.scene_node content)); ] in
  let axis_nodes = List.map (scene_node_of_axis width height t) style.axes in
  let grid_nodes = List.map (scene_node_of_grid width height t) style.grids in
  let legend_nodes = 
    if style.legend && content <> [] then
      [ scene_node_of_legend width height content; ]
    else
      []
  in
    SceneGraph.list (grid_nodes @ axis_nodes @ content_nodes @ legend_nodes)

let scene_node = scene_node_of_frame

let get_data_dimensions_from_total width height frame =
  let bounds =   
    match SceneGraph.bound_scene_node (scene_node width height frame) with
      | Some bound -> bound
      | None -> failwith "bound_frame: empty scenegraph"
  in
    ((width -. (Box.width bounds -. width)),
     (height -. (Box.height bounds -. height)),
     (Box.left bounds),
     (Box.bottom bounds))
