(** Defines the canvas-based elements for user interaction *)
open Color

(** Signature of the module providing the rendering functions *)
module type CanvasUIViewSig = sig
   type data
   val draw    : Dom_html.canvasElement Js.t -> Style.t -> data -> unit
   val default : data
   val updateToMouseMove : data -> float -> float -> data
end

(** Functor to create other UI elements. *)
module CanvasUI(H:CanvasUIViewSig) = struct

   (** Captures the state of the mouse. This will probably end up in it's own module. *)
   type mouse_state =
      {
         mutable pressed : bool;
         mutable pre_x : float;
         mutable pre_y : float;
      }

   (** Contains the main information of the UI element *)
   type elem =
      {
         data   : H.data;                       (** Internal data for the view *)
         canvas : Dom_html.canvasElement Js.t;  (** Canvas of the UI element *)
         style  : Style.t;                      (** Style information use to draw the element *)
         mouse  : mouse_state;                  (** Mouse state information *)
      }

   (**
      This is the main type of the UI element.
      It is a ref because it was hard to do the event stuff from js
      in a more 'pure' way.
   *)
   type t = elem ref

   (** Draws the element *)
   let draw (c:t) =
      H.draw !c.canvas !c.style !c.data

   (** Returns the canvas of the element *)
   let getCanvas (c:t) =
      !c.canvas

   (** Sets the value or data associated with the view. *)
   let set (c:t) (data:H.data) : unit =
      c := { !c with data = data }

   (** Gets the value or data associated with the view. *)
   let get (c:t) : H.data =
      !c.data

   (** Returns the 'id' attribute of the canvas *)
   let getId (c:t) : string =
      Js.to_string (!c.canvas##.id)

   let getOptFloat v : float =
         Js.Optdef.get v (fun () -> 0) |> float_of_int

   (**
      Updates the mouse information based on a mouse event.
      Returns the difference in x and y axis.
   *)
   let updateMousePos e mouse : float * float =
      let x = getOptFloat e##.pageX in
      let y = getOptFloat e##.pageY in
      let diff_x = x -. mouse.pre_x in
      let diff_y = y -. mouse.pre_y in
      mouse.pre_x   <- x;
      mouse.pre_y   <- y;
      diff_x,diff_y

   (** Handles mouse up events *)
   let mouseUpHandler (c:t) =
      Dom_html.handler @@
         fun e ->
            !c.mouse.pressed <- false;
            Js._true

   (** Handles mouse up events *)
   let mouseDownHandler (c:t) =
      Dom_html.handler @@
         fun (e:Dom_html.mouseEvent Js.t) ->
            !c.mouse.pressed <- true;
            let _ = updateMousePos e !c.mouse in
            Js._true

   (** Handles moving the mouse *)
   let mouseMoveHandler (c:t) =
      Dom_html.handler @@
         fun (e:Dom_html.mouseEvent Js.t) ->
            if !c.mouse.pressed = true then
               begin
                  let diff_x,diff_y = updateMousePos e !c.mouse in
                  set c (H.updateToMouseMove (get c) diff_x diff_y);
                  draw c;
                  ()
               end;
            Js._true

   let mouseWheelHandler (c:t) =
      Dom_html.handler @@
         fun (e:Dom_html.mousewheelEvent Js.t) ->
            let diff_x = getOptFloat e##.wheelDeltaX |> ( *. ) 0.3 in
            let diff_y = getOptFloat e##.wheelDeltaY |> ( *. ) 0.3 in
            set c (H.updateToMouseMove (get c) diff_x diff_y);
            draw c;
            Js._true

   (** Creates a new element *)
   let create (id:string) (height:float) (width:float) : t =
      (** Creates the canvas and sets the properties *)
      let canvas = Dom_html.createCanvas Dom_html.window##.document in
      canvas##.id     := Js.string id;
      canvas##.width  := int_of_float width;
      canvas##.height := int_of_float height;

      (* Creates the main object (which is a ref) *)
      let elem =
         ref
         {
            data   = H.default;
            canvas = canvas;
            style  = Style.default;
            mouse  = { pressed = false; pre_x = 0. ;pre_y = 0. }
         }
      in
      (** Adds the event handlers for the mouse *)
      let _ =
         (* Attached to the 'document' otherwise the event is not triggered
            when the button is released outside the canvas *)
         Dom_html.addEventListener
            Dom_html.window##.document
            Dom_html.Event.mouseup
            (mouseUpHandler elem)
            Js._false
      in
      let _ =
         (* This one is attached to the canvas so it only triggers over the element *)
         Dom_html.addEventListener
            canvas
            Dom_html.Event.mousedown
            (mouseDownHandler elem)
            Js._false
      in
      let _ =
         (* Attached to the 'document' otherwise the event is not triggered
            when the mouse is moved outside the canvas *)
         Dom_html.addEventListener
            Dom_html.window##.document
            Dom_html.Event.mousemove
            (mouseMoveHandler elem)
            Js._false
      in
      let _ =
         (* This one is attached to the canvas so it only triggers over the element *)
         Dom_html.addEventListener
            canvas
            Dom_html.Event.mousewheel
            (mouseWheelHandler elem)
            Js._false
      in
      elem

end

(** Renders a knob  *)
module CanvasUIKnobView = struct

   (** The data is simply a float value *)
   type data = float

   (** Defaults to the center *)
   let default = 0.5

   (** Draws the knob *)
   let draw (canvas:Dom_html.canvasElement Js.t) (style:Style.t) (data:data) =
      (** Function to rotate the angle and convert it to radians *)
      let fixAngle x =
         x +. 90. |> Math.rad
      in
      let h = float_of_int (canvas##.height) in
      let w = float_of_int (canvas##.width) in
      (** Center of the ring *)
      let x = h /. 2. in
      let y = w /. 2. in
      (** Makes the radius a little bit smaller than the canvas *)
      let r = (min x y) -. 5. in
      (** Magic number that defines the angle that the ring does not cover *)
      let bottom = 50. in
      (** Calculates the angles *)
      let start_angle = bottom /. 2. in
      let end_angle   = start_angle +. (360. -. bottom) in
      let end_angle_value = end_angle *. data  in

      (** Gets the context *)
      let context = canvas##getContext Dom_html._2d_ in

      (** Draws the background and clears the whole canvas *)
      context##.fillStyle := Color.getString style.Style.background;
      Draw.solidRect context 0. 0. h w;

      (** Draws the ring with some transparency *)
      context##.fillStyle := Color.getString (Color.setAlpha 0.4 style.Style.foreground);
      Draw.solidArc_center context y x r (fixAngle start_angle) (fixAngle end_angle);

      (** Draws the ring (if necessary) *)
      if start_angle < end_angle_value then begin
         context##.fillStyle := Color.getString style.Style.foreground;
         Draw.solidArc_center context y x r (fixAngle start_angle) (fixAngle end_angle_value);
      end;

      (** Draws the center *)
      context##.fillStyle := Color.getString style.Style.background;
      Draw.solidCircle_center context y x (r *. 0.6);
      ()

   (** Updates the value based on the mouse move differences *)
   let updateToMouseMove (data:data) (diff_x:float) (diff_y:float) : data =
      data -. (diff_y *. 0.01) |> min 1.0 |> max 0.0

end


(** Renders a slider  *)
module CanvasUIVSliderView = struct

   (** The data is simply a float value *)
   type data = float

   (** Defaults to the center *)
   let default = 0.5

   (** Draws the knob *)
   let draw (canvas:Dom_html.canvasElement Js.t) (style:Style.t) (data:data) =
      let h = float_of_int (canvas##.height) in
      let w = float_of_int (canvas##.width) in

      (** Margins *)
      let x = 5. in
      let y = 5. in

      let data_i = 1. -. data in

      let large = y +. (data_i *. (h -. 2. *. y)) in

      (** Gets the context *)
      let context = canvas##getContext Dom_html._2d_ in

      (** Draws the background and clears the whole canvas *)
      context##.fillStyle := Color.getString style.Style.background;
      Draw.solidRect context 0. 0. h w;

      context##.fillStyle := Color.getString (Color.setAlpha 0.4 style.Style.foreground);
      Draw.solidRoundRect context x y (w -. x) (h -. y) 15.;

      if data_i > 0.0 then begin
         context##.fillStyle := Color.getString style.Style.foreground;
         Draw.solidRoundRect context large x (h -. y) (w -. x) 15.;
      end;

      ()

   (** Updates the value based on the mouse move differences *)
   let updateToMouseMove (data:data) (diff_x:float) (diff_y:float) : data =
      data -. (diff_y *. 0.01) |> min 1.0 |> max 0.0

end

(** Instantiates the Knob module *)
module CanvasUIKnob = CanvasUI(CanvasUIKnobView)
module CanvasUIVSlider = CanvasUI(CanvasUIVSliderView)

