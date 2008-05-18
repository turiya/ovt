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
    | Horizontal 
    | Vertical

let opposite = function
  | Horizontal -> Vertical
  | Vertical -> Horizontal

let orient point ~orientation = 
  match orientation with
    | Horizontal -> point, 0.
    | Vertical -> 0., point

let orient2 ~orientation a b = 
  match orientation with
    | Horizontal -> a, b
    | Vertical -> b, a

let select (x, y) = function
  | Horizontal -> x
  | Vertical -> y

let string_of = function
  | Horizontal -> "Horizontal"
  | Vertical -> "Vertical"
