(** Defines the drawing style of the UI elements *)
open Color

(** Set of colors defining the style *)
type t =
   {
      background : Color.t;
      foreground : Color.t;
   }

(** Default style of the elements *)
let default =
   {
      background = Color.Name Color.Dimgray;
      foreground = Color.Name Color.Orange;
   }