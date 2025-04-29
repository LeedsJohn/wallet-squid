open! Core

module Note : sig
  type t =
    { name : string
    ; tags : Set.M(Tag).t
    }
  [@@deriving compare, sexp_of]

  include Comparator.S with type t := t
end

type t = Note.t [@@deriving compare, sexp_of]
type comparator_witness = Note.comparator_witness

val comparator : (t, comparator_witness) Comparator.t

(** Recursively searches the directory at base_path to find all notes ending with ".md".
    Returns an error if any of the notes have invalid tags. *)
val load : Base_path.t -> Set.M(Note).t Or_error.t

(** Only used for testing. *)
module Internal : sig
  val make_all : (string * string) list -> Tag_dag.t -> Set.M(Note).t Or_error.t
end
