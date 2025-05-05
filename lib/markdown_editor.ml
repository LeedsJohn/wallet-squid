open! Core

let environment_variable_name = "WALLET_SQUID_MARKDOWN_EDITOR"

type t = string

let param =
  let%map_open.Command base_path =
    flag
      "-markdown-editor"
      (optional string)
      ~doc:
        [%string
          "string Name of program to open markdown file with. If not specified, \n\
          \           will attempt to read the environment variable \
           %{environment_variable_name}.\n\
          \           Defaults to vim if the flag is not provided and the environment \
           variable does not exist."]
  in
  match base_path with
  | Some res -> res
  | None ->
    (match Sys.getenv environment_variable_name with
     | None -> "vim"
     | Some s -> s)
;;
