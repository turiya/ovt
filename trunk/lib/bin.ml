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

type t = Interval.t * float

let interval (i, v) = i
let value (i, v) = v

let to_box (i, v) =
  Box.bound_points ((Interval.lower i), 0.) ((Interval.upper i), v)

let area bin = Box.area (to_box bin)
let bin_area = area

type histogram = t list

let normalize_histogram ?(area=1.) bins =
  let bin_areas = List.map bin_area bins in
  let total_area = List.fold_left (+.) 0. bin_areas in
    if total_area = 0. then
      bins
    else
      List.map (fun (i, v) -> (i, (v *. (area /. total_area)))) bins

let points_in_interval points interval =
  List.filter (Interval.contains interval) points
let count_in_interval points interval =
  List.length (points_in_interval points interval)
let counts_in_intervals points intervals =
  List.map (count_in_interval points) intervals

let histogram ?normalize ~intervals points =
  let counts = List.map float_of_int (counts_in_intervals points intervals) in
  let raw_bins = List.combine intervals counts in
    match normalize with
      | None -> raw_bins
      | Some area -> normalize_histogram ~area:area raw_bins
