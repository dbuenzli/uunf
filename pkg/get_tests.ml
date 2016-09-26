#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let tests_file = "test/NormalizationTest.txt"
let tests_uri = (* FIXME this should source from %%UNICODE_VERSION%% *)
  "http://www.unicode.org/Public/UCD/latest/ucd/NormalizationTest.txt"

let get_tests () =
  OS.File.exists tests_file >>= function
  | true -> Topkg.Ok ()
  | false ->
      let curl = Cmd.(v "curl" % "-f" % "-#" % "-S" % tests_uri) in
      Log.app (fun m -> m "Downloading %s" tests_uri);
      OS.Cmd.(run_out curl |> to_file tests_file)

let main () =
  begin
    Topkg.Private.disable_main (); (* We only want the nicer OS API. *)
    get_tests () >>= fun () -> Ok 0
  end
  |> Log.on_error_msg ~use:(fun () -> 1)

let () = exit (main ())
