open! Core

type t

val make : Set.M(Note).t -> t Or_error.t

(** Returns a list of tags sorted first by frequency and then alphabetically *)
val to_sorted_freq_list : t -> (Tag.t * int) list

(** Get all notes that fall under the tag *)
val find : t -> Tag.t -> Set.M(Note).t
