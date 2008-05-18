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

type point1 = float
type point2 = float * float
type vector2 = float * float

let diff (ax, ay) (bx, by) = ((ax -. bx), (ay -. by))
let sum (ax, ay) (bx, by) = ((ax +. bx), (ay +. by))
let emul (ax, ay) (bx, by) = ((ax *. bx), (ay *. by))
let rotate angle point =
  let x, y = point 
  and c = cos angle
  and s = sin angle in
    ((x *. c -. y *. s), (y *. c +. x *. s))
