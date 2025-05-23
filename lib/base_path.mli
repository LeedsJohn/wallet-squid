open! Core

(** Directory containing notes. *)
type t

(** Returns an error if the provided path is not a directory. *)
val of_string : string -> t Or_error.t

(** A file contains a note if it ends with ".md". Recursively searches the base path
    directory. *)
val get_note_filenames : t -> Set.M(Filename).t

val to_filename : t -> Filename.t
val arg_type : t Command.Arg_type.t

(** Reads a -base-path flag if provided. If not, attempts to read the environment
    variable [WALLET_SQUID_BASE_PATH]. *)
val param : t Command.Param.t
