#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg.ml"

let uutf = Env.bool "uutf"
let cmdliner = Env.bool "cmdliner"

let () =
  Pkg.describe "uunf" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/uunf";
    Pkg.lib ~cond:uutf ~exts:Exts.module_library "src/uunf_string";
    Pkg.bin ~cond:(uutf && cmdliner) ~auto:true "test/unftrip";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md";
    Pkg.doc "DEVEL.md"; ]
