(** A few basic function to draw in a canvas context *)

(** Short type for the canvas context *)
type context = Dom_html.canvasRenderingContext2D Js.t

(** Draws a solid arc (like a pie) given the center of the shape *)
let solidArc_center (context:context) (x:float) (y:float) (r:float) (a_start:float) (a_end:float) : unit =
   context##beginPath;
   context##moveTo x y;
   context##arc x y r a_start a_end Js._false;
   context##lineTo x y;
   context##fill;
   ()

(** Draws a solid circle given the center of the shape *)
let solidCircle_center (context:context) (x:float) (y:float) (r:float) : unit =
   context##beginPath;
   context##arc x y r 0. (Math.pi *. 2.) Js._false;
   context##fill;
   ()

(** Draws a solid rectagle given the corner of the shape *)
let solidRect (context:context) (x:float) (y:float) (h:float) (w:float) : unit =
   context##rect x y h w;
   context##fill;
   ()