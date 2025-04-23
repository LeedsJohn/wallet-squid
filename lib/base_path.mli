open! Core

(** directory containing notes *)
type t

(** returns error case if the provided path is not a directory *)
val of_string : string -> t Or_error.t

val to_filename : t -> Filename.t
val arg_type : t Command.Arg_type.t
val param : t Command.Param.t
