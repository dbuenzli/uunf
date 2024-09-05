open B0_kit.V000
open Result.Syntax

let unicode_version = 15, 1, 0, None (* Adjust on new releases *)
let next_major = let maj, _, _, _ = unicode_version in (maj + 1), 0, 0, None

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let unix = B0_ocaml.libname "unix"
let uucd = B0_ocaml.libname "uucd"
let uutf = B0_ocaml.libname "uutf"
let cmdliner = B0_ocaml.libname "cmdliner"

let uunf = B0_ocaml.libname "uunf"

(* Libraries *)

let uunf_lib = B0_ocaml.lib uunf ~doc:"The uunf library" ~srcs:[ `Dir ~/"src" ]
let uunf_string_lib =
  B0_ocaml.deprecated_lib ~exports:[uunf] (B0_ocaml.libname "uunf.string")

(* Data generation. *)

let generate_data =
  let doc = "uunf_data.ml generator" in
  let srcs =
    [ `Dir ~/"support";
      `File ~/"src/uunf_tmapbool.ml";
      `File ~/"src/uunf_tmapbyte.ml";
      `File ~/"src/uunf_tmap.ml";
      `File ~/"src/uunf_fmt.ml" ]
  in
  let requires = [ uucd; unix ] in
  let meta = B0_meta.(empty |> tag build |> ~~ B0_unit.Action.cwd `Scope_dir) in
  B0_ocaml.exe "generate-data" ~doc ~srcs ~requires ~meta

(* Tools *)

let unftrip =
  let srcs = [ `File ~/"test/unftrip.ml" ] in
  let requires = [ cmdliner; uutf; uunf ] in
  B0_ocaml.exe "unftrip" ~public:true ~doc:"The unftrip tool" ~srcs ~requires

(* Tests *)

let test =
  let srcs = [ `File ~/"test/test_uunf.ml" ] in
  let meta =
    B0_meta.(empty |> tag test |> tag run |> ~~ B0_unit.Action.cwd `Scope_dir)
  in
  let requires = [ b0_std; uunf ] in
  B0_ocaml.exe "test_uunf" ~srcs ~meta ~requires ~doc:"Test normalization"

let examples =
  let srcs = [ `File ~/"test/examples.ml" ] in
  let meta = B0_meta.empty |> B0_meta.(tag test) in
  B0_ocaml.exe "examples" ~srcs ~meta ~requires:[uunf] ~doc:"Doc samples"

(* Actions *)

let uc_base = "http://www.unicode.org/Public"

let show_version =
  B0_unit.of_action "unicode-version" ~doc:"Show supported unicode version" @@
  fun _ _ ~args:_ ->
  Ok (Log.app (fun m -> m "%s" (String.of_version unicode_version)))

let download_tests =
  let doc = "Download the UCD normalization tests" in
  B0_unit.of_action "download-tests" ~doc @@ fun env _ ~args:_ ->
  let version = String.of_version unicode_version in
  let test_url = Fmt.str "%s/%s/ucd/NormalizationTest.txt" uc_base version in
  let test_file = B0_env.in_scope_dir env ~/"test/NormalizationTest.txt" in
  (Log.app @@ fun m ->
   m "@[<v>Downloading %s@,to %a@]" test_url Fpath.pp test_file);
  B0_action_kit.fetch_url env test_url test_file

let download_ucdxml =
  let doc = "Download the ucdxml to support/ucd.xml" in
  B0_unit.of_action "download-ucdxml" ~doc @@ fun env _ ~args:_ ->
  let* unzip = B0_env.get_cmd env (Cmd.tool "unzip") in
  let version = String.of_version unicode_version in
  let ucd_url = Fmt.str "%s/%s/ucdxml/ucd.all.grouped.zip" uc_base version in
  let ucd_file = B0_env.in_scope_dir env ~/"support/ucd.xml" in
  Result.join @@ Os.File.with_tmp_fd @@ fun tmpfile tmpfd ->
  (Log.app @@ fun m ->
   m "@[<v>Downloading %s@,to %a@]" ucd_url Fpath.pp ucd_file);
  let* () = B0_action_kit.fetch_url env ucd_url tmpfile in
  let stdout = Os.Cmd.out_file ~force:true ~make_path:true ucd_file in
  Os.Cmd.run Cmd.(unzip % "-p" %% path tmpfile) ~stdout

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The uunf programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/uunf"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/uunf/doc/Uunf"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/uunf.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/uunf/issues"
    |> ~~ B0_meta.description_tags
      ["unicode"; "text"; "normalization"; "org:erratique"]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
         "--with-uutf" "%{uutf:installed}%"
         "--with-cmdliner" "%{cmdliner:installed}%" ]]|}
    |> ~~ B0_opam.depopts [ "uutf", ""; "cmdliner", ""]
    |> ~~ B0_opam.conflicts
      [ "uutf", {|< "1.0.0"|};
        "cmdliner", {|< "1.1.0"|}]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uucd",
        Fmt.str {|dev & >= "%s" & < "%s"|}
          (String.of_version unicode_version)
          (String.of_version next_major)
      ]
    |> ~~ B0_opam.file_addendum
      [ `Field ("post-messages", `L (true, [
            `S "If the build fails with \"ocamlopt.opt got signal and \
                exited\", issue 'ulimit -s unlimited' and retry.";
            `Raw {|{failure & (arch = "ppc64" | arch = "arm64")}|}]))
      ]
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"uunf package" ~meta ~locked:true @@
  B0_unit.list ()
