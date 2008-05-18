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

(** Axes and ticks. *)

(** A sequence of tick marks along an axis. *)
module Ticks : sig
  type t =
      { offsets: Interval.point_generator; 
        (** Indicate where ticks should appear in data coordinates. *)
        label: bool; (** Indicate whether numeric labels should be drawn. *) }

  (** {3 Construction} *)

  val default : t
end

type t = 
    { edge: Box.edge; (** Edge of frame to which axis is attached. *)
      ticks: Ticks.t list; (** Ticks appearing along the axis. *)
      label: string option; (** An optional label for the whole axis. *) }

(** {3 Construction} *)

val default_x : t
val default_y : t


(** {3 Rendering} *)

(** Build a {!SceneGraph.node} from the axis. *)
val scene_node : t -> Box.t -> Transformation.t -> SceneGraph.node
