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
open Ovt.Interval

let unit = bound_points 0. 1.
let neg_unit = bound_points (-.1.) 0.

let string_of_list string_of_element l = 
  let rec iter result = function
    | [] -> result ^ "]"
    | hd :: tl -> iter (result ^ (string_of_element hd) ^ "; ") tl
  in
    iter "[ " l

let rec assert_equal_list ?(cmp=(=)) ?printer ?msg ~expected ~actual =
  let get_error_string () =
    match printer, msg with 
	  | None, None -> "not equal"
      | None, Some s -> (Format.sprintf "%s\nnot equal" s)
      | Some p, None -> (Format.sprintf "expected: %s but got: %s"
		                   (string_of_list p expected) 
                           (string_of_list p actual))
      | Some p, Some s -> (Format.sprintf "%s\nexpected: %s but got: %s" 
			                 s (string_of_list p expected) 
                             (string_of_list p actual)) in
  let assert_equal_element expected actual =
    if not (cmp expected actual) then
      assert_failure (get_error_string ())
  in
    try List.iter2 assert_equal_element expected actual
    with Invalid_argument _ -> assert_failure (get_error_string ())

let assert_equal_float_list =
  assert_equal_list
    ~cmp:(cmp_float ~epsilon:0.0001) 
    ~printer:string_of_float

let test_generate_periodic _ =
  assert_equal_float_list 
    [ 0.0; 0.3; 0.6; 0.9; ]
    (generate_periodic ~origin:0.0 ~period:0.3 unit);

  assert_equal_float_list
    [ 0.1; 0.4; 0.7; 1.; ]
    (generate_periodic ~origin:0.4 ~period:0.3 unit);

  assert_equal_float_list
    [ 0.1; 0.4; 0.7; 1.; ]
    (generate_periodic ~origin:(-.0.2) ~period:0.3 unit);

  assert_equal_float_list
    [ 0.1; 0.4; 0.7; 1.; ]
    (generate_periodic ~origin:1.3 ~period:0.3 unit);

  assert_equal_float_list
    [ (-.0.8); (-.0.5); (-.0.2); ]
    (generate_periodic ~origin:0.4 ~period:0.3 neg_unit);

  assert_equal_float_list
    [ (-.0.8); (-.0.5); (-.0.2); ]
    (generate_periodic ~origin:(-.0.2) ~period:0.3 neg_unit);

  assert_equal_float_list
    [ (-.0.8); (-.0.5); (-.0.2); ]
    (generate_periodic ~origin:(-.1.1) ~period:0.3 neg_unit)

let suite = "interval" >::: 
  [ "generate_periodic" >:: test_generate_periodic; ]
