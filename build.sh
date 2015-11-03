#!/bin/bash
ocamlbuild -use-ocamlfind main.byte
js_of_ocaml main.byte
