(** A few basic function to draw in a canvas context *)

(** Short type for the canvas context *)
type context = Dom_html.canvasRenderingContext2D Js.t

(** Draws a solid arc (like a pie) given the center of the shape *)
let solidArc_center (context:context) (y:float) (x:float) (r:float) (a_start:float) (a_end:float) : unit =
   context##beginPath;
   context##moveTo y x;
   context##arc y x r a_start a_end Js._false;
   context##lineTo y x;
   context##fill;
   ()

(** Draws a solid circle given the center of the shape *)
let solidCircle_center (context:context) (y:float) (x:float) (r:float) : unit =
   context##beginPath;
   context##arc y x r 0. (Math.pi *. 2.) Js._false;
   context##fill;
   ()

(** Draws a solid rectagle given the corner of the shape *)
let solidRect (context:context) (y:float) (x:float) (h:float) (w:float) : unit =
   context##rect x y w h;
   context##fill;
   ()

let solidRoundRect (context:context) (y:float) (x:float) (h:float) (w:float) (r:float) : unit =
   let up    = 3. /. 4. *. 2. *. Math.pi in
   let right = 0. in
   let left  = Math.pi in
   let down  = Math.pi /. 2. in
   context##beginPath;
   context##arc (x +. r) (y +. r) r up left Js._true;
   context##lineTo x (h -. r);
   context##arc (x +. r) (h -. r) r left down Js._true;
   context##lineTo (w -. r) h;
   context##arc (w -. r) (h -. r) r down right Js._true;
   context##lineTo w (y +. r);
   context##arc (w -. r) (y +. r) r right up Js._true;
   context##fill;
   ()
