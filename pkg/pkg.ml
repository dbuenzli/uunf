#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let distrib =
  (* TODO move distrib to b0 *)
  let watermarks = ("UNICODE_VERSION", `String "15.0.0") :: Pkg.watermarks in
  Pkg.distrib ~watermarks ()

let uutf = Conf.with_pkg "uutf"
let cmdliner = Conf.with_pkg "cmdliner"

let () =
  let opams =
    [Pkg.opam_file "opam" ~lint_deps_excluding:(Some ["b0"; "xmlm"; "uucd"])]
  in
  Pkg.describe "uunf" ~opams ~distrib @@ fun c ->
  let uutf = Conf.value c uutf in
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib ~api:["Uunf"] "src/uunf.mllib";
       Pkg.mllib ~cond:uutf "src/uunf_string.mllib" ~dst_dir:"string";
       Pkg.bin ~cond:(uutf && cmdliner) "test/unftrip";
       Pkg.test "test/test" ~args:(Cmd.v "test/NormalizationTest.txt");
       Pkg.test ~cond:uutf "test/test_string";
       Pkg.test ~cond:uutf "test/examples";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "test/examples.ml"; ]
