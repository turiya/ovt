open Ovt
open OvtGtk

(*****************************************************************************)
(* GENERATE DATA *************************************************************)
(*****************************************************************************)
let domain = Interval.bound_points (-. 5.) 5.
let range = Interval.bound_points (-. 5.) 5.

let normal_pdf ?(period=0.1) domain ~mean ~variance = 
  let x = Interval.generate_periodic ~origin:0. ~period domain in
    List.combine x (List.map (Util.normal_pdf ~mean ~variance) x)

let uniform_variate range =
  (Interval.lower range) +. (Random.float (Interval.width range))

(*****************************************************************************)
(* CONFIGURE VISUALIZATION ***************************************************)
(*****************************************************************************)

(* Line: Normal PDFs *)
let make_frame _ = 
  Frame.frame 
    [ 
DataSet.points ~line:`Line ~label: "N(0, 1)" 
        [ (0., 0.); (10., 10.); ];

DataSet.points ~line:`Line ~label: "N(0, 1)" 
        (normal_pdf domain ~mean:0. ~variance:1.);
      DataSet.points ~line:`Line ~label: "N(0, 3)"
        (normal_pdf domain ~mean:0. ~variance:3.); 
      DataSet.points ~label: "N(0, 5)"
        (normal_pdf domain ~mean:0. ~variance:5. ~period:0.5);
      DataSet.points ~line:`Line ~point:`Automatic ~label: "N(0, 7)"
        (normal_pdf domain ~mean:0. ~variance:7. ~period:0.5); ]

(* Line: Dynamic Uniform Variates *)
(* let variates = ref [] *)
(* let add_variate () = *)
(*   variates := uniform_variate range :: !variates *)

(* let make_frame _ = *)
(*   add_variate (); *)
(*   Frame.frame [ DataSet.plot !variates; ] *)

(* Line/Box: Dynamic Normal Variates *)
let variates = ref []
let add_variate () =
  variates := (Util.normal_variate ~mean:0. ~variance:1.) :: !variates

let make_frame _ =
  add_variate ();
  Frame.frame
    [ DataSet.bins
        ~label:(Printf.sprintf "%d variates" (List.length !variates))
        (Bin.histogram
           ~intervals:(Interval.subdivide domain 20)
           ~normalize:1.
           !variates);
      DataSet.points ~line:`Line ~label: "N(0, 1)"
        (normal_pdf domain ~mean:0. ~variance:1.); ]

(*****************************************************************************)
(* RENDER ********************************************************************)
(*****************************************************************************)

(* Static GUI *)
let _ =
  Gui.show (make_frame ());
  Gui.join ()

(* Dynamic GUI *)
let _ =
  Gui.show (make_frame ());
  while Gui.visible () do
    Gui.update (make_frame ());
    Thread.delay 0.01;
  done

(* PNG file *)
(* let _ = *)
(*   print_endline "writing demo.png"; *)
(*   Render.render_frame "demo.png" (make_frame ()) *)

(* PDF file *)
(* let _ = *)
(*   print_endline "writing demo.pdf"; *)
(*   Render.render_frame "demo.pdf" (make_frame ()) *)
