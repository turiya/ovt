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

(** Data representation: paths, boxes, etc.

    DataSets adorn user data with display instructions.
*)

(** The abstract type of datasets. *)
type t

(** {3 Builders} *)

type auto_line =
    [ Style.line 
    | `Automatic ]

type auto_point =
    [ Style.point 
    | `Automatic ]

val points :
  ?line:auto_line -> ?point:auto_point ->
  ?color:Color.t -> ?label:string -> (float * float) list -> t
val boxes : ?label:string -> Box.t list -> t
val scatter : ?label:string -> (float * float) list -> t
val plot : ?label:string -> ?color:Color.t -> float list -> t
val bins : ?label:string -> Bin.t list -> t

(** {3 Attributes} *)

val color : t -> Color.t Attribute.automatic option
val shape : t -> Style.point Attribute.automatic option

(** {3 Rendering} *)

(** Create a {!SceneGraph.node} to represent the data. *)
val scene_node : t -> SceneGraph.node

(** Create a legend entry to summarize the data. *)
val legend_entry : t -> Legend.entry

(** Bound the contained data values. *)
val bound : t -> Box.t option
