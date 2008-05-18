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

type t =
    { offsets: Interval.point_generator; 
      orientation: Orientation.t; }

let grid ?(offsets = Interval.Count 20) orientation =
  { offsets = offsets;
    orientation = orientation; }
    
let scene_node grid box =
  let make_segment =
    let a, b =
      let i = Box.interval grid.orientation box in
        ((Interval.lower i), (Interval.upper i))
    in
      fun offset ->
        SceneGraph.segment 
          (Orientation.orient2 grid.orientation a offset)  
          (Orientation.orient2 grid.orientation b offset)
  and offsets = 
    Interval.generate_points
      (Box.interval (Orientation.opposite grid.orientation) box)
      grid.offsets
  in
    SceneGraph.list (List.map make_segment offsets)
