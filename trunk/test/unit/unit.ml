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

open OUnit
(* open Ovt *)

(* let assert_equal_intervals a b = *)
(*   assert_equal ~cmp:cmp_float (Interval.lower a) (Interval.lower b); *)
(*   assert_equal ~cmp:cmp_float (Interval.upper a) (Interval.upper b) *)

(* let assert_equal_boxes a b = *)
(*   assert_equal_intervals a.Box.x b.Box.x; *)
(*   assert_equal_intervals a.Box.y b.Box.y *)

(* let assert_equal_optional_boxes a b = *)
(*   match a, b with *)
(*     | Some(a), Some(b) -> assert_equal_boxes a b *)
(*     | None, None -> () *)
(*     | _, _ -> assert_failure "assert_equal_optional_boxes failed"           *)

let suite = "ovt" >::: 
  [ IntervalTest.suite; ]

let _ = 
  run_test_tt ~verbose:true suite
