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

type 'a t = 'a ref

let create initial = ref initial
let get attribute = !attribute
let set attribute value = attribute := value

type 'a automatic_value =
    | Automatic of 'a
    | Assigned of 'a

type 'a automatic = 'a automatic_value t

let automatic default = create (Automatic default)
let assigned value = create (Assigned value)

let is_automatic attribute = 
  match get attribute with
    | Automatic _ -> true
    | Assigned _ -> false

let is_assigned attribute = 
  match get attribute with
    | Automatic _ -> false
    | Assigned _ -> true

let get_automatic attribute =
  match get attribute with
    | Automatic default -> default
    | Assigned value -> value

let set_automatic attribute value = 
  set attribute (Assigned value)
