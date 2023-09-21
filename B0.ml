open B0_kit.V000
open Result.Syntax

let unicode_version = 15, 1, 0, None (* Adjust on new releases *)
let next_major = let maj, _, _, _ = unicode_version in (maj + 1), 0, 0, None

(* OCaml library names *)

let unix = B0_ocaml.libname "unix"
let uucd = B0_ocaml.libname "uucd"
let uutf = B0_ocaml.libname "uutf"
let cmdliner = B0_ocaml.libname "cmdliner"

let uunf = B0_ocaml.libname "uunf"

(* Libraries *)

let uunf_lib = B0_ocaml.lib uunf ~doc:"The uunf library" ~srcs:[ `Dir ~/"src" ]

let uunf_string_lib =
  let represents = [uunf] in
  B0_ocaml.deprecated_lib ~represents (B0_ocaml.libname "uunf.string")

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
  let meta =
    B0_meta.empty
    |> B0_meta.tag B0_meta.build
    |> ~~ B0_unit.Exec.cwd `Scope_dir
  in
  B0_ocaml.exe "generate-data" ~doc ~srcs ~requires ~meta

(* Tools *)

let unftrip =
  let srcs = [ `File ~/"test/unftrip.ml" ] in
  let requires = [ cmdliner; uutf; uunf ] in
  B0_ocaml.exe "unftrip" ~public:true ~doc:"The unftrip tool" ~srcs ~requires

(* Tests *)

let test =
  let srcs = [ `File ~/"test/test.ml" ] in
  let meta =
    B0_meta.empty
    |> B0_meta.tag B0_meta.test
    |> ~~ B0_unit.Exec.cwd `Scope_dir
  in
  let requires = [ uunf ] in
  B0_ocaml.exe "test" ~doc:"Test normalization" ~srcs ~meta ~requires

let examples =
  let srcs = [ `File ~/"test/examples.ml" ] in
  let meta = B0_meta.empty |> B0_meta.(tag test) in
  let requires = [ uunf ] in
  B0_ocaml.exe "examples" ~doc:"Doc samples" ~srcs ~meta ~requires

(* Actions *)

let uc_base = "http://www.unicode.org/Public"

let unzip env = B0_env.get_cmd env (Cmd.arg "unzip")
let curl env =
  B0_env.get_cmd env @@
  Cmd.(arg "curl" % "--fail" % "--show-error" % "--progress-bar" % "--location")

let show_version =
  B0_action.make' "unicode-version" ~doc:"Show supported unicode version" @@
  fun _ _ ~args:_ ->
  Ok (Log.app (fun m -> m "%s" (String.of_version unicode_version)))

let download_tests =
  B0_action.make' "download-tests" ~doc:"Download the UCD normalization tests"@@
  fun _ env ~args:_ ->
  let* curl = curl env in
  let version = String.of_version unicode_version in
  let test_uri = Fmt.str "%s/%s/ucd/NormalizationTest.txt" uc_base version in
  let test_file = B0_env.in_scope_dir env ~/"test/NormalizationTest.txt" in
  (Log.app @@ fun m ->
   m "@[<v>Downloading %s@,to %a@]" test_uri Fpath.pp test_file);
  let stdout = Os.Cmd.out_file ~force:true ~make_path:true test_file in
  Os.Cmd.run Cmd.(curl % test_uri) ~stdout

let download_ucdxml =
  B0_action.make' "download-ucdxml" ~doc:"Download the ucdxml" @@
  fun _ env ~args:_ ->
  let* curl = curl env and* unzip = unzip env in
  let version = String.of_version unicode_version in
  let ucd_uri = Fmt.str "%s/%s/ucdxml/ucd.all.grouped.zip" uc_base version in
  let ucd_file = Fpath.v "support/ucd.xml" in
  let ucd_file = B0_env.in_scope_dir env ucd_file in
  Result.join @@ Os.File.with_tmp_fd @@ fun tmpfile tmpfd ->
  (Log.app @@ fun m ->
   m "@[<v>Downloading %s@,to %a@]" ucd_uri Fpath.pp ucd_file);
  let* () =
    let stdout = Os.Cmd.out_fd ~close:true tmpfd in
    Os.Cmd.run Cmd.(curl % ucd_uri) ~stdout
  in
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
