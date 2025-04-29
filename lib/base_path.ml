open! Core

let environment_variable_name = "WALLET_SQUID_BASE_PATH"

type t = Filename.t

let of_string path =
  match Sys_unix.is_directory path with
  | `Yes -> Ok path
  | `No | `Unknown ->
    error_s [%message "provided path is not a directory" (path : string)]
;;

let load_environment_variable () =
  match Sys.getenv environment_variable_name with
  | None ->
    error_s
      [%message
        "environment variable does not exist"
          ~environment_variable:(environment_variable_name : string)]
  | Some s ->
    (match of_string s with
     | Ok t -> Ok t
     | Error _ as e ->
       Or_error.tag_s
         e
         ~tag:
           [%message
             "got value from environment variable"
               ~environment_variable:(environment_variable_name : string)])
;;

let to_filename t = t
let of_string_exn path = of_string path |> ok_exn
let arg_type = Command.Arg_type.create of_string_exn

let param =
  let%map_open.Command base_path =
    flag
      "-base-path"
      (optional arg_type)
      ~doc:
        [%string
          "path Path to root of directory containing notes. If it is not provided, it \
           will attempt to read the environment variable %{environment_variable_name}."]
  in
  match base_path with
  | Some res -> res
  | None ->
    (match load_environment_variable () with
     | Ok _ as res -> res
     | Error _ as e ->
       Or_error.tag_s
         e
         ~tag:[%message "no base path provided, reading environment variable"])
    |> ok_exn
;;
