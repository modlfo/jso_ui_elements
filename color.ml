(** Extends the CSS.Color module with a few functions that I need *)

module Color = struct
   include CSS.Color

   (** Sets the transparency level a color *)
   let setAlpha (alpha:float) (c:CSS.Color.t) : CSS.Color.t =
      match c with
      | CSS.Color.Name(n) ->
         let r,g,b = CSS.Color.rgb_of_name n in
         CSS.Color.rgb ~a:alpha r g b
      | CSS.Color.RGB(r,g,b)            -> CSS.Color.RGBA(r,g,b,alpha)
      | CSS.Color.RGB_percent(r,g,b)    -> CSS.Color.RGBA_percent(r,g,b,alpha)
      | CSS.Color.RGBA(r,g,b,_)         -> CSS.Color.RGBA(r,g,b,alpha)
      | CSS.Color.RGBA_percent(r,g,b,_) -> CSS.Color.RGBA_percent(r,g,b,alpha)
      | CSS.Color.HSL(h,s,l)            -> CSS.Color.HSLA(h,s,l,alpha)
      | CSS.Color.HSLA(h,s,l,_)         -> CSS.Color.HSLA(h,s,l,alpha)

   (** Returns the string version of the color ready to assign *)
   let getString (c:CSS.Color.t) : Js.js_string Js.t =
      Js.string (CSS.Color.string_of_t c)

end