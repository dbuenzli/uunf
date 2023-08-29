#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let uutf = Conf.with_pkg "uutf"
let cmdliner = Conf.with_pkg "cmdliner"

let () =
  let opams =
    [Pkg.opam_file "opam" ~lint_deps_excluding:(Some ["b0"; "xmlm"; "uucd"])]
  in
  Pkg.describe "uunf" ~opams @@ fun c ->
  let uutf = Conf.value c uutf in
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib ~api:["Uunf"] "src/uunf.mllib";
       Pkg.mllib "src/uunf_string.mllib" ~dst_dir:"string";
       Pkg.bin ~cond:(uutf && cmdliner) "test/unftrip";
       Pkg.test "test/test" ~args:(Cmd.v "test/NormalizationTest.txt");
       Pkg.test "test/examples";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "test/examples.ml"; ]
