open B0_kit.V000
open Result.Syntax

let unicode_version = 15, 1, 0, None (* Adjust on new releases *)
let next_major = let maj, _, _, _ = unicode_version in (maj + 1), 0, 0, None

(* OCaml library names *)

let uucd = B0_ocaml.libname "uucd"
let uutf = B0_ocaml.libname "uutf"
let cmdliner = B0_ocaml.libname "cmdliner"
let uunf = B0_ocaml.libname "uunf"

(* Libraries *)

let uunf_lib =
  let srcs = [`Dir (Fpath.v "src")] in
  B0_ocaml.lib uunf ~doc:"The uunf library" ~srcs

(* Data generation. *)

let generate_data =
  let srcs = [ `Dir (Fpath.v "support");
               `File (Fpath.v "src/uunf_tmapbool.ml");
               `File (Fpath.v "src/uunf_tmapbyte.ml");
               `File (Fpath.v "src/uunf_tmap.ml");
               `File (Fpath.v "src/uunf_fmt.ml")]
  in
  let requires = [uucd] in
  let meta =
    let scope_dir b u = Fut.return (B0_build.scope_dir b u) in
    B0_meta.(empty |> add B0_unit.Action.exec_cwd scope_dir)
  in
  let doc = "uunf_data.ml generator" in
  B0_ocaml.exe "generate-data" ~doc ~srcs ~requires ~meta

(* Tools *)

let unftrip =
  let srcs = [`File (Fpath.v "test/unftrip.ml")] in
  let requires = [cmdliner; uutf; uunf] in
  B0_ocaml.exe "unftrip" ~doc:"The unftrip tool" ~srcs ~requires

(* Tests *)

let test =
  let srcs = [`File (Fpath.v "test/test.ml")] in
  (* FIXME b0, this is not so good. *)
  let meta =
    let scope_dir b u = Fut.return (B0_build.scope_dir b u) in
    B0_meta.(empty
             |> tag test
             |> add B0_unit.Action.exec_cwd scope_dir)
  in
  let requires = [uunf] in
  B0_ocaml.exe "test" ~doc:"Test normalization" ~srcs ~meta ~requires

let examples =
  let srcs = [`File (Fpath.v "test/examples.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [uunf] in
  B0_ocaml.exe "examples" ~doc:"Doc samples" ~srcs ~meta ~requires

(* Cmdlets *)

let uc_base = "http://www.unicode.org/Public"

let unzip () = Os.Cmd.get (Cmd.arg "unzip")
let curl () =
  Os.Cmd.get @@
  Cmd.(arg "curl" % "--fail" % "--show-error" % "--progress-bar" % "--location")

let show_version =
  B0_cmdlet.v "unicode-version" ~doc:"Show supported unicode version" @@
  fun env _args -> B0_cmdlet.exit_of_result @@
  (Log.app (fun m -> m "%s" (String.of_version unicode_version));
   Ok ())

let download_tests =
  B0_cmdlet.v "download-tests" ~doc:"Download the UCD normalization tests" @@
  fun env _args -> B0_cmdlet.exit_of_result @@
  let version = String.of_version unicode_version in
  let test_uri = Fmt.str "%s/%s/ucd/NormalizationTest.txt" uc_base version in
  let test_file = Fpath.v "test/NormalizationTest.txt" in
  let test_file = B0_cmdlet.in_scope_dir env test_file in
  let stdout = Os.Cmd.out_file ~force:true ~make_path:true test_file in
  let* curl = curl () in
  Log.app (fun m ->
      m "@[<v>Downloading %s@,to %a@]" test_uri Fpath.pp test_file);
  Os.Cmd.run Cmd.(curl % test_uri) ~stdout

let download_ucdxml =
  B0_cmdlet.v "download-ucdxml" ~doc:"Download the ucdxml" @@
  fun env _args -> B0_cmdlet.exit_of_result @@
  let version = String.of_version unicode_version in
  let ucd_uri = Fmt.str "%s/%s/ucdxml/ucd.all.grouped.zip" uc_base version in
  let ucd_file = Fpath.v "support/ucd.xml" in
  let ucd_file = B0_cmdlet.in_scope_dir env ucd_file in
  let* curl = curl () and* unzip = unzip () in
  Result.join @@ Os.File.with_tmp_fd @@ fun tmpfile tmpfd ->
  Log.app (fun m ->
      m "@[<v>Downloading %s@,to %a@]" ucd_uri Fpath.pp ucd_file);
  let stdout = Os.Cmd.out_fd ~close:true tmpfd in
  let* () = Os.Cmd.run Cmd.(curl % ucd_uri) ~stdout in
  let stdout = Os.Cmd.out_file ~force:true ~make_path:true ucd_file in
  let* () = Os.Cmd.run Cmd.(unzip % "-p" %% path tmpfile) ~stdout in
  Ok ()

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
    |> add B0_opam.Meta.conflicts
      [ "uutf", {|< "1.0.0"|};
        "cmdliner", {|< "1.1.0"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "b0", {|dev & >= "0.0.5" |};
        "uucd", Fmt.str {|dev & >= "%s" & < "%s"|}
        (String.of_version unicode_version) (String.of_version next_major)
      ]
    |> add B0_opam.Meta.file_addendum
      [ `Field ("post-messages", `L (true, [
            `S "If the build fails with \"ocamlopt.opt got signal and \
                exited\", issue 'ulimit -s unlimited' and retry.";
            `Raw {|{failure & (arch = "ppc64" | arch = "arm64")}|}]))]
  in
  B0_pack.v "default" ~doc:"uunf package" ~meta ~locked:true @@
  B0_unit.list ()
