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
    | Identity
    | Scale of Point.point2
    | Translate of Point.point2
    | Compose of t * t

let rec invert = function
  | Identity as i -> i
  | Scale (sx, sy) -> Scale ((1. /. sx), (1. /. sy))
  | Translate (dx, dy) -> Translate ((-. dx), (-. dy))
  | Compose (ta, tb) -> Compose ((invert tb), (invert ta))

let rec transform_point transformation point =
  match transformation with
    | Identity -> point
    | Scale scale -> Point.emul scale point
    | Translate translation -> Point.sum translation point
    | Compose (ta, tb) -> transform_point ta (transform_point tb point)

let rec transform_box transformation box =
  match transformation with
    | Identity -> box
    | Scale s -> 
        Box.bound_points
          (transform_point transformation (Box.lower_left box))
          (transform_point transformation (Box.upper_right box))
    | Translate translation -> Box.translate box translation
    | Compose (ta, tb) -> transform_box ta (transform_box tb box)

let rec transform_fixed_box transformation box =
  match transformation with
    | Identity -> box
    | Scale s -> 
        let ll = (Box.lower_left box) in
        let offset = Point.diff (Point.emul s ll) ll in
          transform_fixed_box (Translate offset) box
    | Translate translation -> Box.translate box translation
    | Compose (ta, tb) -> transform_fixed_box ta (transform_fixed_box tb box)
