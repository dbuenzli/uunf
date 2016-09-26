#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let massage () =
  let ocaml = Conf.tool "ocaml" `Build_os in
  OS.Cmd.run Cmd.(ocaml % "pkg/build_support.ml") >>= fun () ->
  OS.Cmd.run Cmd.(ocaml % "pkg/get_tests.ml")

let distrib =
  (* FIXME OPAMv2, move this to an x-unicode-version field in the opam file. *)
  let watermarks = ("UNICODE_VERSION", `String "9.0.0") :: Pkg.watermarks in
  let exclude_paths () =
    Pkg.exclude_paths () >>| fun ps ->
    ("test/NormalizationTest.txt" :: "support" :: ps)
  in
  Pkg.distrib ~watermarks ~massage ~exclude_paths ()

let uutf = Conf.with_pkg "uutf"
let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "uunf" ~distrib @@ fun c ->
  let uutf = Conf.value c uutf in
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib ~api:["Uunf"] "src/uunf.mllib";
       Pkg.mllib ~cond:uutf "src/uunf_string.mllib";
       Pkg.bin ~cond:(uutf && cmdliner) "test/unftrip";
       Pkg.test "test/test" ~args:(Cmd.v "test/NormalizationTest.txt");
       Pkg.test ~cond:uutf "test/test_string";
       Pkg.test "test/examples";
       Pkg.doc "DEVEL.md";
       Pkg.doc "test/examples.ml"; ]
