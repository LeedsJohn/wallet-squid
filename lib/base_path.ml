open! Core

type t = Filename.t

let of_string path =
  match Sys_unix.is_directory path with
  | `Yes -> Ok path
  | `No | `Unknown ->
    error_s [%message "provided path is not a directory" (path : string)]
;;

let to_filename t = t
let of_string_exn path = of_string path |> ok_exn
let arg_type = Command.Arg_type.create of_string_exn
