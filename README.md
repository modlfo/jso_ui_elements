# jso_ui_elements
Test of making UI elements completely in Ocaml (js_of_ocaml)

Note: this is not a full library (yet). I'm just playing with js_of_ocaml and learning how to make web apps with ocaml.

### Requirements

- Ocaml compiler >= 4.02
- js_of_ocaml >= 2.6 (with ppx syntax extension)

### Building

Use the build.sh script or run:
```
$ ocamlbuild -use-ocamlfind main.byte
$ js_of_ocaml main.byte
```
