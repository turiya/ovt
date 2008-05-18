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
open Point
open Transformation

module A = struct
  module Box = struct
    type attributes =
        { color: Color.t;
          thickness: float; }

    type t = Box.t * attributes

    let attributes ~color ~thickness =
      { color = color;
        thickness = thickness; }

    let create ~color ~thickness box =
      `Box (box, (attributes ~color ~thickness))

    let render cr (box, a) =
      let r, g, b = a.color in
      let x = Box.left box
      and y = Box.bottom box
      and w = Box.width box
      and h = Box.height box in
        Cairo.rectangle cr x y w h;
        Cairo.set_antialias cr Cairo.ANTIALIAS_NONE;
        Cairo.set_source_rgb cr r g b;
        Cairo.fill_preserve cr;
        Cairo.set_line_width cr a.thickness;
        Cairo.set_source_rgb cr 0. 0. 0.;
        Cairo.stroke cr
  end
    
  module Point = struct
    type attributes =
        { color: Color.t;
          shape: Style.point;
          area: float; }

    type t = point2 list * attributes

    let attributes ~color ~shape ~area =
      { color = color;
        shape = shape;
        area = area; }

    let create ~color ~shape ~area point =
      `Point (point, (attributes ~color ~shape ~area))

    let render cr (p, a) =
      let render_point source (x, y) = 
        Cairo.save cr;
        Cairo.translate cr (x -. 10.) (y -. 10.);
        Cairo.set_source cr source;
        Cairo.rectangle cr 5. 5. 10. 10.;
        Cairo.fill cr;
        Cairo.restore cr
      and make_path = function
        | `Circle ->
            Cairo.arc cr 0. 0. a.area 0. Util.m_2_pi;
            Cairo.close_path cr;
        | `Square ->
            let d = a.area in
              Cairo.rectangle cr (-. d) (-. d) (2. *. d) (2. *. d);
        | `Diamond ->
            let d = a.area in
              Cairo.rotate cr (Util.m_pi /. 4.);
              Cairo.rectangle cr (-. d) (-. d) (2. *. d) (2. *. d);
      and r, g, b = a.color in
        Cairo.push_group cr;
        Cairo.translate cr 10. 10.;
        make_path a.shape;
        Cairo.set_source_rgb cr r g b;
        Cairo.fill cr;
        List.iter (render_point (Cairo.pop_group cr)) p
          (*             Cairo.set_source_rgb cr 0. 0. 0.; *)
        (*             Cairo.set_line_width cr 1.; *)
        (*             Cairo.stroke cr; *)
  end

  module Segment = struct
    type attributes = 
        { color: Color.t; 
          thickness: float; }

    type t = point2 * point2 * attributes

    let attributes ~color ~thickness =
      { color = color; 
        thickness = thickness; }

    let create ~color ~thickness a b =
      `Segment (a, b, (attributes ~color ~thickness))

    let render cr ((ax, ay), (bx, by), a) =
      let r, g, b = a.color in
        Cairo.move_to cr ax ay;
        Cairo.line_to cr bx by;
        Cairo.set_antialias cr Cairo.ANTIALIAS_NONE;
        Cairo.set_source_rgb cr r g b;
        Cairo.set_line_width cr a.thickness;
        Cairo.stroke cr
  end

  module Text = struct
    type weight = 
        | Normal
        | Bold

    type attributes =
        { color: Color.t;
          orientation: Orientation.t;
          size: float;
          weight: weight; }

    type t = point2 * string * attributes

    let render cr ((x, y), text, a) =
      let r, g, b = a.color in
      let orient () = match a.orientation with 
        | Horizontal -> ()
        | Vertical -> Cairo.rotate cr (-. Util.m_pi /. 2.) 
      in
        Cairo.move_to cr x y;
        Cairo.identity_matrix cr;
        orient ();
        Cairo.set_source_rgb cr r g b;
        let weight = 
          match a.weight with
            | Normal -> Cairo.FONT_WEIGHT_NORMAL
            | Bold -> Cairo.FONT_WEIGHT_BOLD
        in
          Cairo.select_font_face cr "sans" Cairo.FONT_SLANT_NORMAL weight;
          Cairo.set_font_size cr a.size;
          Cairo.show_text cr text
  end

  module Tick = struct
    type attributes = 
        { orientation: Orientation.t; 
          length: float; }

    type t = point2 * attributes

    let attributes ~orientation ~length =
      { orientation = orientation;
        length = length; }

    let create ~orientation ~length point =
      `Tick (point, (attributes ~orientation ~length))

    let render cr ((x, y), a) =
      let dx, dy = match a.orientation with
        | Horizontal -> a.length, 0.
        | Vertical -> 0., a.length 
      in
        Cairo.move_to cr (x -. dx) (y -. dy);
        Cairo.line_to cr (x +. dx) (y +. dy);
        Cairo.set_antialias cr Cairo.ANTIALIAS_NONE;
        Cairo.set_line_width cr 1.;
        Cairo.stroke cr;
  end

  module Path = struct
    type attributes =
        { color: Color.t; 
          thickness: float; }

    type t = point2 list * attributes

    let attributes ~color ~thickness =
      { color = color; 
        thickness = thickness; }

    let create ~color ~thickness points =
      `Path (points, (attributes ~color ~thickness))

    let render cr (points, a) =
      match points with
        | [] -> ()
        | (x, y) :: tl -> 
            let add_to_path (x, y) = Cairo.line_to cr x y in
              Cairo.move_to cr x y;
              List.iter add_to_path tl;
              let r, g, b = a.color in
                Cairo.set_source_rgb cr r g b;
                Cairo.set_line_width cr a.thickness;
                Cairo.stroke cr
  end
end

let box = A.Box.create
let point = A.Point.create
let segment = A.Segment.create
let tick = A.Tick.create
let path = A.Path.create

type t =
    [ `Box of A.Box.t
    | `Point of A.Point.t
    | `Segment of A.Segment.t
    | `Text of A.Text.t
    | `Tick of A.Tick.t
    | `Path of A.Path.t ]

let bound_text_cr = 
  Cairo.create (Cairo.image_surface_create Cairo.FORMAT_ARGB32 1 1)

let bound = function
  | `Box (x, _) -> Some x
  | `Point (p, _) -> 
      let bound_point x =
        let d = (4., 4.) in
        let a = Point.sum x d in
        let b = Point.diff x d in
          Box.bound_points a b
      in
        Box.bound_box_list (List.map bound_point p)
  | `Segment (xa, xb, _) -> Some (Box.bound_points xa xb)
  | `Text (x, text, a) -> 
      let (xa, xb) =
        let orient = match a.A.Text.orientation with
          | Horizontal -> (fun x y -> x, (-. y))
          | Vertical -> (fun x y -> y, x)
        and extents = 
          let weight = 
            match a.A.Text.weight with
              | A.Text.Normal -> Cairo.FONT_WEIGHT_NORMAL
              | A.Text.Bold -> Cairo.FONT_WEIGHT_BOLD
          in
            Cairo.select_font_face 
              bound_text_cr "sans" Cairo.FONT_SLANT_NORMAL weight;
            Cairo.set_font_size bound_text_cr a.A.Text.size;
            Cairo.text_extents bound_text_cr text 
        in
          ((orient extents.Cairo.x_bearing extents.Cairo.y_bearing),
           (orient extents.Cairo.x_advance extents.Cairo.y_advance))
      in
        Some (Box.translate (Box.bound_points xa xb) x)
  | `Tick (x, a) -> 
      let length = 2. in
      let d = match a.A.Tick.orientation with
        | Horizontal -> length, 0.
        | Vertical -> 0., length in
      let a = Point.sum x d in
      let b = Point.diff x d in
        Some (Box.bound_points a b)
  | `Path (p, _) -> Box.bound_box_list (List.map Box.bound_point p)

exception TransformationError
let transform transformation primitive =
  match primitive with
    | `Box (box, attributes) -> 
        let a = transform_point transformation (Box.lower_left box)
        and b = transform_point transformation (Box.upper_right box) in
          `Box ((Box.bound_points a b), attributes)
    | `Point (p, attributes) -> 
        `Point ((List.map (transform_point transformation) p), attributes)
    | `Segment (a, b, attributes) -> 
        `Segment ((transform_point transformation a),
                  (transform_point transformation b),
                  attributes)
    | `Text (p, text, attributes) -> 
        let bound = match bound primitive with
          | None -> raise TransformationError
          | Some bound -> bound in
        let transformed = transform_fixed_box transformation bound in
        let error = Point.diff p ((Box.left bound), (Box.top bound)) in
        let anchor = (Box.left transformed), (Box.top transformed) in
          `Text ((Point.sum anchor error), text, attributes)
    | `Tick (x, attributes) -> 
        `Tick ((transform_point transformation x), attributes)
    | `Path (p, attributes) -> 
        `Path ((List.map (transform_point transformation) p), attributes)
