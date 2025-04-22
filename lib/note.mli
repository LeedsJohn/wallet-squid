open! Core

module Note : sig
  type t [@@deriving compare, sexp_of]

  include Comparator.S with type t := t
end

type t = Note.t [@@deriving compare, sexp_of]

include Comparator.S with type t := t

(** Recursively searches the directory at base_path to find all notes ending with ".md"
    Returns an error if any of the notes have invalid tags.
 *)
val load : base_path:string -> Set.M(Note).t Or_error.t
