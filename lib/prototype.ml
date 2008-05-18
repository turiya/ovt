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

(* Some general design principles 

   Goal: Visualization passes through three distinct stages. At each,
   all the information necessary to move to the next stage is
   contained entirely in the vocabulary of a single stage. The best
   names so far for these stages are concept, layout, and
   primitive.

   Concepts represent the visualization abstractly: some data is
   represented in a scatter plot, the data points are labeled with
   their coordinates, and axes are labeled and visible.

   Layout puts the concepts in physical relation to one another: data
   is scaled within some frame, labels appear above and to the right
   of the points they decorate, and axes appear along the left and
   bottom.

   Primitives give the layout explicit coordinates and can be easily
   rendered to cairo.

   Current state: Concepts and layout are muddled. I don't know what
   layout operations I need in order to cleanly split concepts from
   layout. It would be nice to define a layout structure along the
   lines of a scene graph and completely translate from concepts to
   layout types before moving on to convert layout to primitives.

   Primitives are sound; scene graphs represent primitives and simple
   transformations.

   ---
   
   Goal: Interaction with concepts should occur in whatever
   dimension is most natural. Axes are inherently one-dimensional
   objects; that ticks and labels must be placed in two-dimensional
   space is simply a layout detail.

   Current state: Some concepts honor this idea; TickList represents
   ticks one-dimensional coordinates and takes the second coordinate
   as a separate parameter. At the level of an individual tick, this
   just isn't feasible. Each must be placed at some two-dimensional
   coordinate. Better separating concepts from layout might make this
   cleaner, as well.

   ---

   Goal: Types clearly reflect the three distinct stages of
   visualization and are meaningfully named.

   Current state: Types related to concepts and layout are poorly
   named. They express (currently) necessary distinctions, but the
   names are largely arbitrary. The description of information and
   processes by the types is somewhat arbitrary and quite
   delicate. There is some reason why Ticks are created as a
   decoration while a TickList is a generator, but the reason is not
   immediately clear. Worse, the delicate division of information and
   processes makes it difficult to place new concepts in the
   hierarchy.

   Primitives and the scene graph are reasonably named. 
*)

type point2 = float * float

open Orientation
open Transformation
open Placement


(* let _ = Gtk.main () *)
