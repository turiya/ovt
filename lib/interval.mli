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

(** Interval operations.

    An interval represents all numbers between its lower and upper
    endpoints.  Each endpoint is either closed or open. If the
    endpoint is closed, the interval includes the value of the
    endpoint; if the endpoint is open, the interval does not.
*)

(** The abstract type of intervals. *)
type t

type endpoint =
    | Open (** The interval does not include the endpoint. *)
    | Closed (** The interval includes the endpoint. *)

(** {3 Builders } *) 

(** Build an interval with the given lower and upper endpoints and
    endpoint types. lower must be less than or equal to upper. *)
val interval : 
  lower:float -> upper:float -> 
  lower_end:endpoint -> upper_end:endpoint -> t

(** Build a closed interval with the given lower and upper endpoints.
    lower must be less than or equal to upper. *)
val closed_interval : lower:float -> upper:float -> t 

(** Build an open interval with the given lower and upper endpoints.
    lower must be strictly less than upper. *)
val open_interval : lower:float -> upper:float -> t

(** {3 Bounds }

    Bounding functions return the smallest interval which contains the
    given arguments.
*)

(** Bounds two intervals a and b. *)
val bound_intervals : t -> t -> t

(** Bound every interval in a list. Since the list may be empty, the
    result is of type [t option]. *)
val bound_interval_list : t list -> t option

(** Bound a single point a. *)
val bound_point : float -> t

(** Bound two points a and b. *)
val bound_points : float -> float -> t

(** Bound every point in a list. Since the list may be empty, the
    result is of type [t option]. *)
val bound_point_list : float list -> t option

(** {3 Accessors } *)

(** Lower endpoint. *)
val lower : t -> float

(** Upper endpoint. *)
val upper : t -> float

(** Open/Closed of lower endpoint. *)
val lower_end : t -> endpoint

(** Open/Closed of upper endpoint. *)
val upper_end : t -> endpoint

(** Width. [width i = (upper i) -. (lower i)] *)
val width : t -> float

(** Center. [center i = ((lower i) +. (upper i)) /. 2.] *)
val center : t -> float

(** {3 Operations } *)

(** Determine whether the interval contains the given point. *)
val contains : t -> float -> bool

(** Extend the interval by the given amounts. *)
val extend : ?lower:float -> ?upper:float -> ?both:float -> t -> t

(** Intersection. Since the intersection may be empty, the result is
    of type [t option]. *)
val intersect : t -> t -> t option

(** Translation. For [f] in [\{ lower, upper \}], [(f i) +. x = f
    (translate i x)] *)
val translate : t -> float -> t

(** Subdivision. [subdivide i n] divides [i] in to [n] intervals of
    equal length.  Excepting the first and last, resulting intervals
    are closed on the left and open on the right. The first result is
    closed on the left only if [i] is; the last is open on the right
    only if [i] is. *)
val subdivide : t -> int -> t list

(** {3 Point Generation} *)

(** Generate points separated by a fixed period in the interval. *)
val generate_periodic : origin:float -> period:float -> t -> float list

type point_generator =
    | Fixed of float list
    | Periodic of float * float
    | Count of int

val generate_points : t -> point_generator -> float list
