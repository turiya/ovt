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

module Ticks = struct
  type t =
      { offsets: Interval.point_generator;
        label: bool; }

  let default = 
    { offsets = Interval.Count 20;
      label = true; }

  let tick_node orientation place point =
    place (SceneGraph.tick (Orientation.opposite orientation) point)

  let label_node edge place offset tick =
    let extra_tick = place tick ~anchor:tick in
    let padding = SceneGraph.Padding.none in
    let padding = 
      SceneGraph.Padding.extend_edge (Box.opposite edge) 4. padding in
    let padding = 
      let orientation = Box.orientation_of_edge edge in
      let amount = match orientation with
          | Orientation.Horizontal -> 4.
          | Orientation.Vertical -> 8.
      in
        SceneGraph.Padding.extend_orientation orientation amount padding in
    let text =
      place 
        (SceneGraph.Padding
           (padding,
            (SceneGraph.text (0., 0.) (Printf.sprintf "%.2f" offset))))
        ~anchor:extra_tick
    in
      SceneGraph.list [ extra_tick; text; ]

  let scene_node edge interval place_tick place_label transformation ticks =
    let offsets = Interval.generate_points interval ticks.offsets in
    let oriented_offsets = 
      List.map (Orientation.orient ~orientation:(Box.orientation_of_edge edge)) offsets in
    let tick_nodes = 
      List.map 
        (tick_node (Box.orientation_of_edge edge) place_tick)
        (List.map (Transformation.transform_point transformation) oriented_offsets)
    in
    let label_nodes = 
      if ticks.label
      then List.map2 (label_node edge place_label) offsets tick_nodes
      else [] 
    in
      SceneGraph.list 
        [ SceneGraph.list tick_nodes; 
          SceneGraph.OverlapFilter label_nodes; ]
end

type t = 
    { edge: Box.edge;
      ticks: Ticks.t list; 
      label: string option; }

let default_x = 
  { edge = Box.Bottom;
    ticks = [ Ticks.default ];
    label = None; }

let default_y = 
  { edge = Box.Left;
    ticks = [ Ticks.default ];
    label = None; }

let scene_node_of_label text edge segment ticks =
  let label = 
    SceneGraph.Padding
      ((SceneGraph.Padding.extend_edge (Box.opposite edge) 8. 
          SceneGraph.Padding.none), 
       (SceneGraph.text 
          ~weight:Primitive.A.Text.Bold
          ~orientation:(Box.orientation_of_edge edge) (0., 0.) text)) 
  in
    match edge with
      | Box.Left -> 
          SceneGraph.place 
            (SceneGraph.place label LeftOf ticks) 
            CenteredV 
            segment
      | Box.Right -> 
          SceneGraph.place 
            (SceneGraph.place label RightOf ticks)
            CenteredV 
            segment
      | Box.Top -> 
          SceneGraph.place
            (SceneGraph.place label Above ticks)
            CenteredH            
            segment
      | Box.Bottom -> 
          SceneGraph.place
            (SceneGraph.place label Below ticks)
            CenteredH
            segment

let outside_placement = function
  | Box.Left -> LeftOf
  | Box.Right -> RightOf
  | Box.Top -> Above
  | Box.Bottom -> Below

let centered_placement = function
  | Orientation.Vertical -> CenteredV
  | Orientation.Horizontal -> CenteredH

let flip_vertical = function
  | Box.Bottom -> Box.Top
  | Box.Top -> Box.Bottom
  | Box.Left | Box.Right as e -> e

let scene_node axis box (transformation as t) =
  let nodes = [] in
  let orientation = Box.orientation_of_edge axis.edge in
  let segment = 
    let a, b = Box.edge axis.edge box in
      SceneGraph.segment a b; in
  let nodes = segment :: nodes in
  let ticks = 
    let place_tick =
      let f = SceneGraph.place ~anchor:segment in
        f ~placement:(outside_placement axis.edge)
    and place_label =
      let f = SceneGraph.place2 in
        f ~horizontal:(outside_placement axis.edge)
          ~vertical:(centered_placement orientation)
    and interval =
      Box.interval orientation 
        (Transformation.transform_box (Transformation.invert t) box)
    in
      SceneGraph.list 
        (List.map 
           (Ticks.scene_node axis.edge interval place_tick place_label transformation)
           axis.ticks) in
  let nodes = ticks :: nodes in
  let nodes = 
    match axis.label with
      | None -> nodes
      | Some label -> 
          (scene_node_of_label label axis.edge segment ticks) :: nodes 
  in
    SceneGraph.list nodes
