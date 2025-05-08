open! Core

type t =
  { name : string
  ; tags : Set.M(Tag).t
  ; content : string
  }
[@@deriving compare, sexp_of]

include Comparator.S with type t := t

(** Errors if the note contains an invalid tag. *)
val make : Raw_note_content.t -> Tag_dag.t -> t Or_error.t
