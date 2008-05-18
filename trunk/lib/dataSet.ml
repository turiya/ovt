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

open Point

module Points = struct
  type style =
      { line: Style.line Attribute.automatic option;
        point: Style.point Attribute.automatic option;
        color: Color.t Attribute.automatic;
        label: string option; }

  type t = point2 list * style

  let color (_, s) = s.color
  let shape (_, s) = s.point

  let scene_node (p, style) =
    let color = Attribute.get_automatic style.color in
    let points = match style.point with
      | None -> []
      | Some point -> 
          [ SceneGraph.point 
              ~shape:(Attribute.get_automatic point) 
              ~color:color
              p; ]
    and lines = match style.line with
      | None -> []
      | Some linestyle -> [ SceneGraph.path ~thickness:2. ~color:color p ]
    in SceneGraph.list (lines @ points)

  let swatch style =
    let color = Attribute.get_automatic style.color in
    let line = match style.line with
      | None -> []
      | Some _ -> 
          [ SceneGraph.segment ~thickness:2. ~color:color (0., 0.) (16., 0.); ]
    and point = match style.point with
      | None -> []
      | Some point -> [ SceneGraph.point 
                          ~shape:(Attribute.get_automatic point)
                          ~color:color 
                          [ (8., 0.); ]; ]
    in
      SceneGraph.list (line @ point)

  let legend_entry (_, style) =
    { Legend.
        swatch = (swatch style);
        label = match style.label with
          | None -> "Points"
          | Some label -> label; }

  let bound (p, _) = Box.bound_point_list p
end

module Boxes = struct
  type style = 
      { label: string; }

  type t = Box.t list * style

  let scene_node (b, s) = 
    SceneGraph.list (List.map (SceneGraph.box ~thickness:2.) b)

  let legend_entry (_, s) =
    { Legend.
        swatch = 
          SceneGraph.box ~thickness:2. (Box.bound_points (0., 0.) (14., 8.));
        label = s.label; }
      
  let bound (b, _) = Box.bound_box_list b
end

type t =
    | Points of Points.t
    | Boxes of Boxes.t

let scene_node = function
  | Points p -> Points.scene_node p
  | Boxes b -> Boxes.scene_node b
      
let legend_entry = function
  | Points p -> Points.legend_entry p
  | Boxes b -> Boxes.legend_entry b

let bound = function
  | Points p -> Points.bound p
  | Boxes b -> Boxes.bound b

let color = function
  | Points p -> Some (Points.color p)
  | Boxes b -> None

let shape = function
  | Points p -> Points.shape p
  | Boxes b -> None

type auto_line =
    [ Style.line 
    | `Automatic ]

type auto_point =
    [ Style.point 
    | `Automatic ]

let points ?line ?point ?color ?label data =
  let point = match (line, point) with
    | (None, None) -> Some `Automatic
    | (_, p) -> p in
  let point = match point with
    | None -> None
    | Some point -> 
        begin match point with
          | `Automatic -> Some (Attribute.automatic `Circle)
          | #Style.point as value -> Some (Attribute.assigned value)
        end in
  let line = match line with
    | None -> None
    | Some line -> 
        begin match line with
          | `Automatic -> Some (Attribute.automatic `Line)
          | #Style.line as value -> Some (Attribute.assigned value)
        end in
  let color = match color with
    | None -> Attribute.automatic Color.black
    | Some color -> Attribute.assigned color 
  in
    Points (data,
            { Points.line = line;
              Points.point = point;
              Points.color = color;
              Points.label = label; })

let boxes ?(label="Boxes") data = 
  Boxes (data,
         { Boxes.label = label; })

let scatter ?(label="Scatter") p = points ~label:label ~point:`Circle p

let plot ?label ?color p = 
  let rec enumerate ?(index=0.) = function
    | [] -> []
    | hd :: tl -> (index, hd) :: (enumerate ~index:(index +. 1.) tl)
  in
    points ?label ?color ~line:`Line (enumerate p)

let bins ?(label="Bins") b = boxes ~label:label (List.map Bin.to_box b)
