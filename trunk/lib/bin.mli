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

(** Bins and histograms.

    A bin associates an interval with a value. Bins are used to
    represent barcharts and histograms.
*)

type t = Interval.t * float

(** Interval. *)
val interval : t -> Interval.t

(** Value. *)
val value : t -> float

(** Create a box from a bin [b] with x coordinates given by [(interval
    b)]. The y coordinates are [0.] and [(value b)]. *)
val to_box : t -> Box.t

(** Area of the box defined by the bin. *)
val area : t -> float

(** {3 Histograms } *)

type histogram = t list

(** Normalize a histogram so that the area of its constituent bins is
    equal to [area]. If the area of the given histogram is 0, it is
    returned unchanged.

    @param area default value [1.]
*)
val normalize_histogram : ?area:float -> histogram -> histogram

(** Build a histogram from a list of intervals and a list of points.

    @param normalize If present, normalize result by calling
    [normalize_histogram]. The given value is passed as the [area]
    argument.

    @param intervals Count points in these intervals. Points outside
    the given intervals are ignored. *)
val histogram : 
  ?normalize:float -> intervals:Interval.t list -> float list -> histogram
