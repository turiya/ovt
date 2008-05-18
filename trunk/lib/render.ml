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

open Orientation
open Transformation

let string_of_point2 (x, y) =
  Printf.sprintf "(%f, %f)" x y

module Text = struct
  let string_of_primitive primitive = 
    let sprintf = Printf.sprintf in
      match primitive with
        | `Box (x, _) -> 
            sprintf "PrimitiveBox%s" (Box.string_of_box x)
        | `Point (p, _) -> 
            let sp = String.concat "; " (List.map string_of_point2 p) in
              sprintf "Point([ %s])" sp
        | `Segment (xa, xb, _) -> 
            let sxa = string_of_point2 xa
            and sxb = string_of_point2 xb in
              sprintf "Segment(%s, %s)" sxa sxb
        | `Text (x, text, a) -> 
            sprintf "Primitive.Text(%s, \"%s\")" (string_of_point2 x) text
        | `Tick (x, a) -> 
            let sx = string_of_point2 x
            and so = Orientation.string_of a.Primitive.A.Tick.orientation in
              sprintf "Tick(%s, %s)" sx so
        | `Path (p, _) ->
            let sp = String.concat "; " (List.map string_of_point2 p) in
              sprintf "Path([ %s])" sp

  let render_frame width height frame =
    let scene_node = Frame.scene_node_of_frame width height frame in
    let primitives = SceneGraph.flatten_scene_node scene_node in
    let strings = List.map string_of_primitive primitives in
      List.iter print_endline strings
end

module Cairo = struct
  let render_primitive cr primitive = 
    let render cr = function
      | `Box a -> Primitive.A.Box.render cr a
      | `Point a -> Primitive.A.Point.render cr a
      | `Segment a-> Primitive.A.Segment.render cr a 
      | `Text a -> Primitive.A.Text.render cr a
      | `Tick a -> Primitive.A.Tick.render cr a 
      | `Path a -> Primitive.A.Path.render cr a
    in
      Cairo.save cr;
      render cr primitive;
      Cairo.restore cr

  let render_scene_node cr width height node =
    Cairo.scale cr 1. (-. 1.);
    Cairo.translate cr 0. (float (-height));
    List.iter (render_primitive cr) (SceneGraph.flatten_scene_node node)

  let fit_frame width height frame =
    let width = (float_of_int (width - 32)) in
    let height = (float_of_int (height - 32)) in
    let w, h, xo, yo = Frame.get_data_dimensions_from_total width height frame in
    let frame = Frame.scene_node_of_frame w h frame in
      SceneGraph.transform
        (Transformation.Translate ((-. xo +. 16.), (-. yo +. 16.)))
        frame

  let render_frame cr ~width ~height frame =
    render_scene_node cr width height (fit_frame width height frame)

  let render_scene_node_to_file ~node ~width ~height ~filename =
    let cr = 
      Cairo.create 
        (Cairo.image_surface_create Cairo.FORMAT_ARGB32 width height)
    in
      Cairo.set_source_rgb cr 1. 1. 1.;
      Cairo.paint cr;
      Cairo.set_source_rgb cr 0. 0. 0.;
      render_scene_node cr width height node;
      Cairo_png.surface_write_to_file (Cairo.get_target cr) filename

  let render_frame_to_file ~frame ~width ~height ~filename =
    let node = fit_frame width height frame in
      render_scene_node_to_file node width height filename

  let render_scene_node_to_pdf ~node ~width ~height filename =
    let file = open_out filename in
    let surface = 
      Cairo_pdf.surface_create_for_channel 
          file (float width) (float height) in
    let cr = Cairo.create surface in
      Cairo.set_source_rgb cr 1. 1. 1.;
      Cairo.paint cr;
      Cairo.set_source_rgb cr 0. 0. 0.;
      render_scene_node cr width height node;
      Cairo.show_page cr;
      Cairo.surface_finish surface;
      close_out file

  let render_frame_to_pdf ~frame ~width ~height filename =
    let node = fit_frame width height frame in
      render_scene_node_to_pdf node width height filename

end

type format = 
    | PDF
    | PNG

let extension (filename as f) =
  let base = String.length (Filename.chop_extension f) in
    String.sub f (base + 1) ((String.length f) - base - 1)

exception Unknown_format
let infer_format (filename as f) =
  let e = try extension f with Invalid_argument _ -> raise Unknown_format in
    match String.uppercase e with
      | "PDF" -> PDF
      | "PNG" -> PNG
      | _ -> raise Unknown_format

let get_format format filename = 
  match format with
    | None -> infer_format filename 
    | Some f -> f

let render_scene_node ?(width=512) ?(height=512) ?format ~filename node =
  let format = get_format format filename in
    match format with
      | PNG -> Cairo.render_scene_node_to_file ~width ~height ~filename ~node:node
      | PDF -> Cairo.render_scene_node_to_pdf ~node ~width ~height filename

let render_frame ?(width=512) ?(height=512) ?format ~filename frame =
  let format = get_format format filename in
    match format with
      | PNG -> Cairo.render_frame_to_file ~width ~height ~filename ~frame:frame
      | PDF -> Cairo.render_frame_to_pdf ~frame ~width ~height filename
          
