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

type t = { x: Interval.t; y: Interval.t; }

let of_intervals x y =
  { x = x; y = y; }

let bound_points (ax, ay) (bx, by) =
  { x = Interval.bound_points ax bx;
    y = Interval.bound_points ay by; }

let bound_point a = 
  bound_points a a

let bound_boxes a b =
  { x = Interval.bound_intervals a.x b.x;
    y = Interval.bound_intervals a.y b.y; }

let bound_box_list = function
  | [] -> None
  | hd :: tl -> Some (List.fold_left bound_boxes hd tl)

let bound_point_list = function
  | [] -> None
  | hd :: tl -> 
      let bound box point = bound_boxes box (bound_point point) in
        Some(List.fold_left bound (bound_point hd) tl)

let lower_left a = (Interval.lower a.x), (Interval.lower a.y)
let upper_right a = (Interval.upper a.x), (Interval.upper a.y)

let left a = Interval.lower a.x
let right a = Interval.upper a.x
let bottom a = Interval.lower a.y
let top a = Interval.upper a.y

type edge =
    | Left 
    | Right
    | Top
    | Bottom

let opposite = function
  | Left -> Right
  | Right -> Left
  | Top -> Bottom
  | Bottom -> Top

let edge edge box = 
  match edge with
    | Left -> 
        let x = left box in
          ((x, (bottom box)), (x, (top box)))
    | Right ->
        let x = right box in
          ((x, (bottom box)), (x, (top box)))
    | Top ->
        let y = top box in
          (((left box), y), ((right box), y))
    | Bottom ->
        let y = bottom box in
          (((left box), y), ((right box), y))

let orientation_of_edge = function
  | Left | Right -> Orientation.Vertical
  | Top | Bottom -> Orientation.Horizontal

let interval orientation box = 
  match orientation with
    | Orientation.Horizontal -> box.x
    | Orientation.Vertical -> box.y

let width a = Interval.width a.x
let height a = Interval.width a.y
let area a = (width a) *. (height a)

let translate box (dx, dy) =
  { x = Interval.translate box.x dx;
    y = Interval.translate box.y dy; }

let string_of_box a =
  Printf.sprintf "((%f, %f), (%f, %f))" (left a) (bottom a) (right a) (top a)

let contains box (x, y) =
  let x = Interval.contains box.x x 
  and y = Interval.contains box.y y in
    x && y
  
let intersect a b =
  match (Interval.intersect a.x b.x), (Interval.intersect a.y b.y) with
    | (Some x), (Some y) -> Some { x = x; y = y; }
    | _, _ -> None

let overlaps a b = Util.is_some (intersect a b)
