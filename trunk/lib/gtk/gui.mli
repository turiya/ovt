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

(** Simple GUI output.

    This module provides OVT output to a single resizable window with
    GTK. {b These methods are suitable for simple utilities but will
    not work in applications which use GTK elsewhere.} The GTK event
    loop executes in a separate thread so that only the [join] method
    blocks. 
*)

(** Make the window visible and display the given frame. While the
    window is visible, calls to [show] have no effect. *)
val show : Ovt.Frame.t -> unit

(** Update the window to display the given frame.

    @raise Failure if the window is not visible. *)
val update : Ovt.Frame.t -> unit

(** Wait for the user to close the window. Return immediately if the
    window is not visible. *)
val join : unit -> unit

(** @return [true] if the window is visible, [false] otherwise. *)
val visible : unit -> bool
