open! Core

(** A tag.t can only contain lowercase letters, digits, dashes, and underscores. It must
    be between 1 and 64 characters. *)
type t [@@deriving compare, sexp_of]

include Comparator.S with type t := t

(** Returns an error if the string is not composed entirely of letters, digits, dashes,
    and underscores or if the length of the string is not betwen 1 and 64 characters. *)
val of_string : string -> t Or_error.t

val to_string : t -> string
