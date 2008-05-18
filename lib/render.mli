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

(** Render Frames and SceneGraphs to screen or disk. *)

module Cairo : sig
  (** Render a frame to the given cairo context. *)
  val render_frame : Cairo.t -> width:int -> height:int -> Frame.t -> unit
end

type format = 
    | PDF
    | PNG

(** Render a {!SceneGraph.node} to disk. If the format is given
    explicitly, it will be used. Otherwise, [render_scene_node]
    attempts to infer the format from the filename's extension.

    @raise Unknown_format if format is not given and cannot be
    inferred *)
val render_scene_node :
  ?width:int -> ?height:int -> 
  ?format:format -> filename:string -> SceneGraph.node -> unit

(** Render a {!Frame.t} to disk. If the format is given explicitly, it will
    be used. Otherwise, [render_frame] attempts to infer the format
    from the filename's extension.

    @raise Unknown_format if format is not given and cannot be
    inferred *)
val render_frame :
  ?width:int -> ?height:int -> 
  ?format:format -> filename:string -> Frame.t -> unit
