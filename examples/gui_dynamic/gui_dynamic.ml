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

let domain = Interval.bound_points (-. 3.) 3.

let pdf_plot =
  let points =
    let x = Interval.generate_periodic ~origin:0. ~period:0.01 domain in
    let y = List.map (Util.normal_pdf ~mean:0. ~variance:1.) x in
      List.combine x y
  in
    DataSet.points ~line:`Line ~label:"N(0, 1)" ~color:Color.blue points

let make_histogram variates =
  let bins = 
    Bin.histogram 
      ~intervals:(Interval.subdivide domain 20)
      ~normalize:1.
      variates
  and label = Printf.sprintf "%d variates" (List.length variates) in
    DataSet.bins ~label bins
      
let make_frame variates =
    Frame.frame [ (make_histogram variates); pdf_plot; ]

let random_variates = ref []
let add_variate () =
  let x = Util.normal_variate ~mean:0. ~variance:1. in
    random_variates := x :: !random_variates 

let _ = 
  Gui.show (make_frame !random_variates);
  while Gui.visible () do
    add_variate ();
    Gui.update (make_frame !random_variates);
    Thread.delay 0.01;
  done;
