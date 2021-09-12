open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml library names *)

let uunf = B0_ocaml.libname "uunf"
let uunf_string = B0_ocaml.libname "uunf.string"

let uutf = B0_ocaml.libname "uutf"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let uunf_lib =
  let srcs = Fpath.[ `Dir (v "src");
                     `X (v "src/uunf_string.ml");
                     `X (v "src/uunf_string.mli"); ]
  in
  let requires = [] in
  B0_ocaml.lib uunf ~doc:"The uunf library" ~srcs ~requires

let uunf_string_lib =
  let srcs = Fpath.[ `File (v "src/uunf_string.ml");
                     `File (v "src/uunf_string.mli") ]
  in
  let requires = [uunf; uutf] in
  B0_ocaml.lib uunf_string ~doc:"The uunf.string library" ~srcs ~requires

(* Tools *)

let unftrip =
  let srcs = Fpath.[`File (v "test/unftrip.ml")] in
  let requires = [cmdliner; uutf; uunf] in
  B0_ocaml.exe "unftrip" ~doc:"The unftrip tool" ~srcs ~requires

(* Tests *)

let test =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ uunf ] in
  B0_ocaml.exe "test" ~doc:"Test normalization" ~srcs ~meta ~requires

let test_string =
  let srcs = Fpath.[`File (v "test/test_string.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ uutf; uunf; uunf_string ] in
  B0_ocaml.exe "test_string" ~doc:"Test uunf.string" ~srcs ~meta ~requires

let examples =
  let srcs = Fpath.[`File (v "test/examples.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [uutf; uunf] in
  B0_ocaml.exe "examples" ~doc:"Examples" ~srcs ~meta ~requires

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The uunf programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/uunf"
    |> add online_doc "https://erratique.ch/software/uunf/doc/Uunf"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/uunf.git"
    |> add issues "https://github.com/dbuenzli/uunf/issues"
    |> add description_tags
      ["unicode"; "text"; "normalization"; "org:erratique"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
         "--with-uutf" "%{uutf:installed}%"
         "--with-cmdliner" "%{cmdliner:installed}%" ]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depopts [ "uutf", ""; "cmdliner", ""]
    |> add B0_opam.Meta.conflicts [ "uutf", {|< "1.0.0"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.03.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
      ]

  in
  B0_pack.v "default" ~doc:"uunf package" ~meta ~locked:true @@
  B0_unit.list ()
