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

open Ovt
open OvtGtk

let dataset_a = DataSet.points ~line:`Line [ (0., 0.); (10., 10.); ]
let dataset_b = DataSet.points ~line:`Line [ (0., 10.); (10., 0.); ]

let frame_a = Frame.frame [ dataset_a; ]
let frame_b = Frame.frame [ dataset_b; ]
let frame_ab = Frame.frame [ dataset_a; dataset_b; ]

let _ = 
  (* Show a plot and wait for the user to close the window. *)
  Gui.show frame_a;
  Gui.join ();

  (* As soon as the first window is closed, show another. 
     Wait one second and update the window. 
     Finally wait for the user to close the window. *)
  Gui.show frame_b;
  Thread.delay 1.;
  Gui.update frame_ab;
  Gui.join ()
