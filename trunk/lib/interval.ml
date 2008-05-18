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

type endpoint =
    | Open 
    | Closed

type t = 
    { lower: float; 
      upper: float; 
      lower_end: endpoint; 
      upper_end: endpoint; }

let interval ~lower ~upper ~lower_end ~upper_end =
  assert (lower <= upper);
  if lower == upper then 
    assert (lower_end == Closed && upper_end == Closed);
  { lower = lower;
    upper = upper;
    lower_end = lower_end;
    upper_end = upper_end; }

let closed_interval ~lower ~upper = interval lower upper Closed Closed

let open_interval ~lower ~upper = interval lower upper Open Open

let bound_intervals a b = 
  let lower_end =
    if a.lower < b.lower then a.lower_end
    else if b.lower < a.lower then b.lower_end
    else if a.lower_end == Closed || b.lower_end == Closed then Closed
    else Open
  and upper_end =
    if a.upper > b.upper then a.upper_end
    else if b.upper > a.upper then b.upper_end
    else if a.upper_end == Closed || b.upper_end == Closed then Closed
    else Open
  in
    interval (min a.lower b.lower) (max a.upper b.upper) lower_end upper_end

let bound_interval_list = function
  | [] -> None
  | hd :: tl -> Some(List.fold_left bound_intervals hd tl)

let bound_point a = closed_interval a a

let bound_points a b = closed_interval (min a b) (max a b)

let bound_point_list l = bound_interval_list (List.map bound_point l)

let lower a = a.lower
let upper a = a.upper
let lower_end a = a.lower_end
let upper_end a = a.upper_end
let width a = a.upper -. a.lower
let center a = (a.lower +. a.upper) /. 2.

let contains interval point =
  let lower = match interval.lower_end with
    | Open -> point > interval.lower
    | Closed -> point >= interval.lower
  and upper = match interval.upper_end with
    | Open -> point < interval.upper
    | Closed -> point <= interval.upper
  in
    lower && upper

let extend ?(lower=0.) ?(upper=0.) ?(both=0.) interval =
  { interval with
      lower = interval.lower -. (lower +. both);
      upper = interval.upper +. (upper +. both); }

let intersect a b =
  let lower, lower_end =
    if a.lower > b.lower then a.lower, a.lower_end
    else if b.lower > a.lower then b.lower, b.lower_end
    else if a.lower_end == Open || b.lower_end == Open then a.lower, Open
    else a.lower, Closed
  and upper, upper_end =
    if a.upper < b.upper then a.upper, a.upper_end
    else if b.upper < a.upper then b.upper, b.upper_end
    else if a.upper_end == Open || b.upper_end == Open then a.upper, Open
    else a.upper, Closed
  in
    if upper < lower then
      None
    else
      Some (interval lower upper lower_end upper_end)

let translate interval offset =
  { interval with 
      lower = interval.lower +. offset;
      upper = interval.upper +. offset; }

let pop_front l = (List.hd l), (List.tl l)
let pop_back l = let r = List.rev l in (List.hd r), (List.rev (List.tl r))

let subdivide i divisions =
  assert (divisions > 0);
  let w = (width i) /. (float_of_int divisions) in
  let edges = Util.float_range ~step:w i.lower i.upper in
  let _, left_edges = pop_back edges in
  let _, right_edges = pop_front edges in
  let ranges = List.combine left_edges right_edges in
  let (left_lower, left_upper), inner_ranges = pop_front ranges in
  let (right_lower, right_upper), inner_ranges = pop_back inner_ranges in
  let left_interval = 
    interval left_lower left_upper i.lower_end Open in
  let right_interval = 
    interval right_lower right_upper Closed i.upper_end in
  let inner_intervals =
    List.map (fun (l, u) -> interval l u Closed Open) inner_ranges
  in
    [ left_interval ] @ inner_intervals @ [ right_interval ]

let generate_periodic ~origin ~period interval =
  let origin = 
    origin +. ((ceil ((interval.lower -. origin) /. period)) *. period) in
  let rec generate i =
    let x = origin +. ((float i) *. period) in
      if contains interval x then
        x :: (generate (i + 1))
      else
        []
  in
    generate 0

type point_generator =
    | Fixed of float list
    | Periodic of float * float
    | Count of int

let rec generate_points interval generator =
  match generator with
    | Fixed l -> List.filter (contains interval) l
    | Periodic (origin, period) ->
        (* extend the interval slightly to allow for floating point
           error and prevent the offsets from flickering for small
           changes to the interval *)
        let interval = extend ~both:(period *. 0.0001) interval in
          generate_periodic ~origin:origin ~period:period interval
    | Count (count) -> 
        let origin = lower interval in
        let period = (width interval) /. (float count) in
          generate_points interval (Periodic (origin, period))


