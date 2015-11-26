
open Color
open CanvasUI


let document = Dom_html.window##.document

(* Gets the div with id='content' to put stuff there *)
let content =
   Js.Opt.get (document##getElementById(Js.string "content"))
      (fun () -> assert false)

(** Function to be called when the page loads *)
let onload _ =
   (* Creates two knobs *)
   let knob1 = CanvasUIKnob.create "knob1" 100. 100. in
   let slider1 = CanvasUIVSlider.create "slider1" 100. 100. in
   (* Draw them *)
   CanvasUIKnob.draw knob1;
   CanvasUIVSlider.draw slider1;
   (* Put them into the 'content' div *)
   Dom.appendChild content (CanvasUIKnob.getCanvas knob1);
   Dom.appendChild content (CanvasUIVSlider.getCanvas slider1);
   Js._false
;;

(* Does the stuff when the window loads *)
let _ = Dom_html.window##.onload := Dom_html.handler onload


