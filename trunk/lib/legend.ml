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

type entry =
    { swatch: SceneGraph.node;
      label: string; }

let entry swatch label =
  { swatch = swatch;
    label = label; }

type t = entry list

let scene_node_of_entry entry =
  let swatch_bound = 
    SceneGraph.box ~thickness:0. (Box.bound_points (0., 0.) (16., 12.)) in
  let placed_swatch = 
    SceneGraph.place2 entry.swatch CenteredH CenteredV swatch_bound in
  let text = SceneGraph.text (0., 0.) entry.label in
  let padded_text = 
    SceneGraph.Padding 
      ((SceneGraph.Padding.padding ~all:4. ()),
       text) in
  let placed_text = 
    SceneGraph.place2 padded_text RightOf CenteredV swatch_bound 
  in
    SceneGraph.list [ swatch_bound; placed_swatch; placed_text; ]

let scene_node legend =
  match List.map scene_node_of_entry legend with
    | [] -> SceneGraph.list []
    | hd :: [] -> hd
    | hd :: tl -> 
        SceneGraph.Padding 
          ((SceneGraph.Padding.padding ~all:4. ()),
           (SceneGraph.stack tl Below hd))
