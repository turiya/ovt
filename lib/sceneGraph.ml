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

open Placement
open Transformation

module Padding = struct
  type t = { left: float;
             right: float;
             top: float;
             bottom: float; }

  let none = { left = 0.; right = 0.; top = 0.; bottom = 0.; }

  let extend ?(left=0.) ?(right=0.) ?(bottom=0.) ?(top=0.) 
      ?(horizontal=0.) ?(vertical=0.) ?(all=0.) padding =
    { left = padding.left +. left +. horizontal +. all;
      right = padding.right +. right +. horizontal +. all;
      top = padding.top +. top +. vertical +. all;
      bottom = padding.bottom +. bottom +. vertical +. all; }

  let extend_edge edge amount ~padding =
    match edge with
      | Box.Left -> extend ~left:amount padding
      | Box.Right -> extend ~right:amount padding 
      | Box.Bottom -> extend ~bottom:amount padding
      | Box.Top -> extend ~top:amount padding

  let extend_orientation orientation amount ~padding =
    match orientation with
      | Orientation.Horizontal -> extend ~horizontal:amount padding
      | Orientation.Vertical -> extend ~vertical:amount padding

  let padding ?left ?right ?bottom ?top ?horizontal ?vertical ?all () =
    extend ?left ?right ?bottom ?top ?horizontal ?vertical ?all none

  let pad box padding =
    Box.of_intervals 
      (Interval.extend 
         ~lower:padding.left 
         ~upper:padding.right
         (Box.interval Orientation.Horizontal box))
      (Interval.extend 
         ~lower:padding.bottom 
         ~upper:padding.top
         (Box.interval Orientation.Vertical box))
end

type node =
    | Placement of node * Placement.t * node
    | OverlapFilter of node list
    | Padding of Padding.t * node
    | Primitive of Primitive.t
    | Transform of Transformation.t * node
    | SceneNodeList of node list

let place node ~placement ~anchor = Placement (node, placement, anchor)
let place2 node ~horizontal ~vertical ~anchor = 
  place (place node horizontal anchor) vertical anchor

let box ?(color=Color.white) ?(thickness=1.) box = 
  Primitive (Primitive.box ~color ~thickness box)
let point ?(color=Color.black) ?(shape=`Circle) ?(area=3.) p = 
  Primitive (Primitive.point ~color ~shape ~area p)
let segment ?(color=Color.black) ?(thickness=1.) a b = 
  Primitive (Primitive.segment ~color ~thickness a b)
let text ?(color=Color.black) ?(orientation=Orientation.Horizontal) 
    ?(size=10.) ?(weight=Primitive.A.Text.Normal) a text =
  Primitive (`Text (a, text,
                    { Primitive.A.Text.
                        color = color;
                        orientation = orientation;
                        size = size;
                        weight = weight; }))
let tick ?(length=2.) ~orientation location = 
  Primitive (Primitive.tick ~orientation ~length location)
let path ?(color=Color.black) ?(thickness=2.) p = 
  Primitive (Primitive.path ~color ~thickness p)

let transform transformation node = Transform (transformation, node)

let list l = SceneNodeList l

let rec stack nodes placement anchor =
  match nodes with
    | [] -> anchor
    | hd :: tl -> 
        let placed_hd = place hd placement anchor in
          stack tl placement (SceneNodeList [ placed_hd; anchor; ])

let rec transform_scene_node transformation node = 
  match node with
    | Padding (p, n) ->
        Padding (p, (transform_scene_node transformation n))
    | OverlapFilter nodes ->
        OverlapFilter (List.map (transform_scene_node transformation) nodes)
    | Placement (node, placement, reference) -> 
        let t = transform_scene_node transformation in
          Placement ((t node), placement, (t reference))
    | Primitive primitive -> 
        Primitive (Primitive.transform transformation primitive)
    | Transform (t, node) -> Transform ((Compose (transformation, t)), node)
    | SceneNodeList nodes -> 
        SceneNodeList (List.map (transform_scene_node transformation) nodes)

let rec every_other remove = function
  | [] -> []
  | hd :: tl -> 
      if remove then 
        every_other (not remove) tl
      else 
        hd :: every_other (not remove) tl          


exception PaddingError
exception PlacementError
exception OverlapFilterError

let rec cull_overlaps nodes =
  let bounds = Util.strip_none (List.map bound_scene_node nodes) in
  let pairs = Util.all_pairs_without_replacement bounds in
  let overlaps = List.map (fun (a, b) -> Box.overlaps a b) pairs in
  let any_overlaps = List.fold_left (||) false overlaps in
    if not any_overlaps then
      nodes
    else
      cull_overlaps (every_other false nodes)

and flatten_scene_node = function
  | Padding (p, n) -> 
      let b = match bound_scene_node n with
        | None -> raise PaddingError
        | Some b -> Padding.pad b p
      in
        flatten_scene_node (list [ box ~thickness:0. b; n; ])
  | OverlapFilter nodes -> flatten_scene_node (list (cull_overlaps nodes))
  | Placement (node, placement, reference) -> 
      let reference_bound = 
        match bound_scene_node reference with
          | None -> raise PlacementError
          | Some bound -> bound 
      and node_bound = 
        match bound_scene_node node with
          | None -> raise PlacementError
          | Some bound -> bound in
      let offset = 
        match placement with
          | AlignLeft -> 
              ((Box.left reference_bound) -. (Box.left node_bound)), 0.
          | LeftOf -> 
              ((Box.left reference_bound) -. (Box.right node_bound)), 0.
          | AlignRight ->
              ((Box.right reference_bound) -. (Box.right node_bound)), 0.
          | RightOf -> 
              ((Box.right reference_bound) -. (Box.left node_bound)), 0.
          | AlignTop -> 
              0., ((Box.top reference_bound) -. (Box.top node_bound))
          | Above -> 
              0., ((Box.top reference_bound) -. (Box.bottom node_bound))
          | AlignBottom -> 
              0., ((Box.bottom reference_bound) -. (Box.bottom node_bound))
          | Below -> 
              0., ((Box.bottom reference_bound) -. (Box.top node_bound))
          | CenteredH ->
              ((Interval.center reference_bound.Box.x) 
               -. (Interval.center node_bound.Box.x)),
              0.
          | CenteredV ->
              0.,
              ((Interval.center reference_bound.Box.y) 
               -. (Interval.center node_bound.Box.y))
      in
        flatten_scene_node (transform (Translate offset) node)
  | Primitive primitive -> [ primitive ]
  | Transform (transformation, node) ->
      flatten_scene_node (transform_scene_node transformation node)
  | SceneNodeList nodes -> List.flatten (List.map flatten_scene_node nodes)

and bound_scene_node node =
  Box.bound_box_list 
    (Util.strip_none (List.map Primitive.bound (flatten_scene_node node)))
