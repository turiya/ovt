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

module VersionMap = Map.Make
  (struct
     type t = int
     let compare = compare
   end)

type test_scene = 
    { index: int;
      description: string;
      f: string -> unit; }

let run_test existing test = 
  let version =
    try (VersionMap.find test.index existing) + 1
    with Not_found -> 0
  and save ~file ~string =
    let channel = open_out file in
      output_string channel string;
      close_out channel
  in
    test.f (Printf.sprintf "recent/%d-%d.png" test.index version);
    save
      ~string:test.description
      ~file:(Printf.sprintf "recent/%d-%d.txt" test.index version)

let parse_test_filename f =
  Scanf.sscanf f "%d-%d.png" (fun index version -> index, version)

let add_version map filename =
  try
    let index, version = parse_test_filename filename in
      try
        let existing = VersionMap.find index map in
          VersionMap.add index (max version existing) map
      with
          Not_found -> VersionMap.add index version map
  with
      _ -> map

let run_tests tests =
  let files = Sys.readdir "recent" in
  let existing = Array.fold_left add_version VersionMap.empty files in
    List.iter (run_test existing) tests
