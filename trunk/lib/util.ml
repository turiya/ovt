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

let is_some = function
  | None -> false
  | Some _ -> true

let rec strip_none = function
  | [] -> []
  | hd :: tl -> 
      begin
        match hd with
          | Some(x) -> x :: (strip_none tl)
          | None -> strip_none tl
      end

let m_pi = acos (-. 1.)
let m_2_pi = 2. *. m_pi

let rec float_range ?(step = 1.) a b =
  if a > b then
    []
  else
    a :: (float_range ~step:step (a +. step) b)

let rec cartesian_product a b = 
  match a with
    | [] -> []
    | hd :: tl ->
	    List.map (fun x -> (hd, x)) b 
	    @ cartesian_product tl b

let all_pairs_without_replacement a =
  let rec iter left right pairs =
    match right with
      | [] -> pairs
      | hd :: tl ->
          iter (hd :: left) tl (pairs @ List.map (fun x -> hd, x) (left @ tl))
  in
    iter [] a []
    
let normal_pdf ~mean ~variance x =
  let a = 1. /. (sqrt (2. *. m_pi *. variance)) in
  let b = -. ((x -. mean) ** 2.) /. (2. *. variance) in
    a *. (exp b)

let normal_variate ~mean ~variance =
  let u1 = Random.float 1. in
  let u2 = Random.float 1. in
  let r = sqrt ((-. 2.) *. (log u1)) in
  let t = 2. *. m_pi *. u2 in
    ((r *. (cos t)) -. mean) /. (sqrt variance)

exception No_value

let get = function
  | None -> raise No_value
  | Some x -> x

