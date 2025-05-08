open! Core

type t = Set.M(Note).t

(** Converts every [Raw_note_content.t] to a [Note.t]. This involves applying transitive
    tags. Errors if any note contains an invalid tag. *)
val make : Raw_note_content.t list -> Tag_dag.t -> t Or_error.t

(** Fuzzy search all notes. *)
val fzf : t -> Note.t option

val param : t Command.Param.t
