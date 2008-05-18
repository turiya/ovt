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

(** DataSet containers.

    Frames scale DataSets to the screen and decorate them with axes,
    grids, and legends. *)

(** Transformation from data coordinates to screen coordinates. *)
type scale =
    | Fixed of Transformation.t (** Use the given transformation. *)
    | Fit (** Use a transformation which makes all data visible. *)

type style =
    { scale: scale;
      axes: Axis.t list;
      grids: Grid.t list; 
      legend: bool; }

type content = DataSet.t list

type t = style * content

(** {3 Constructors} *)

val default_style : style

val frame : ?style:style -> content -> t

(** {3 Rendering} *)

(** Build a {!SceneGraph.node} from the frame. Frame contents will be
    completely contained within the the box defined by [(0., 0.),
    (width, height)] though frame decorations may extend beyond it. *)
val scene_node_of_frame : width:float -> height:float -> t -> SceneGraph.node

val get_data_dimensions_from_total : float -> float -> t -> float * float * float * float
