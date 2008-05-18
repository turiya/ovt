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

let make_frame () =
  Frame.frame [ DataSet.points ~line:`Line [ (0., 0.); (10., 10.); ]; ]

let _ = 
  print_endline "writing minimal.png";
  Render.render_frame "minimal.png" (make_frame ());
  print_endline "writing minimal.pdf";
  Render.render_frame "minimal.pdf" (make_frame ());
  print_endline "done"
