#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let ucd_file = "support/ucd.xml"
let ucd_uri =
  "http://www.unicode.org/Public/%%UNICODE_VERSION%%/ucdxml/ucd.all.grouped.zip"

let get_ucd () =
  let zipped = ucd_file ^ ".zip" in
  OS.File.exists ucd_file >>= function
  | true -> Topkg.Ok ()
  | false ->
      let curl = Cmd.(v "curl" % "-f" % "-#" % "-S" % ucd_uri) in
      let funzip = Cmd.(v "funzip" % zipped) in
      Log.app (fun m -> m "Downloading %s" ucd_uri);
      OS.Cmd.(run_out curl |> to_file zipped)
      >>= fun () -> OS.Cmd.(run_out funzip |> to_file ucd_file)
      >>= fun () -> OS.File.delete zipped

let gen_props () =
  let ocb = Conf.tool "ocamlbuild" `Build_os in
  let ocb = Cmd.(ocb % "-classic-display" % "-no-links" % "-use-ocamlfind") in
  let gen_props =
    Cmd.(v "_build/support/gen_props.native" %
         "-norm" % "src/uunf_data.ml")
  in
  (* FIXME this won't work on bytecode only pins *)
  OS.Cmd.run Cmd.(ocb % "gen_props.native")
  >>= fun () -> OS.Cmd.run gen_props

let main () =
  begin
    Topkg.Private.disable_main (); (* We only want the nicer OS API. *)
    get_ucd ()
    >>= fun () -> gen_props ()
    >>= fun () -> Ok 0
  end
  |> Log.on_error_msg ~use:(fun () -> 1)

let () = exit (main ())
