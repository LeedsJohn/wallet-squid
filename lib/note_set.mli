open! Core

type t = Set.M(Note).t

(** Recursively searches the directory at base_path to find all notes ending with ".md".
    Returns an error if any of the notes have invalid tags. *)
val load : Base_path.t -> t Or_error.t

(** Fuzzy search all notes *)
val fzf : t -> Note.t option

val param : t Command.Param.t

module For_testing : sig
  val make : (string * string) list -> Tag_dag.t -> t Or_error.t
end
