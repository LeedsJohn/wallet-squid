open! Core

type t

val make : Set.M(Note).t -> t

(** Returns a list of tags sorted first by frequency and then alphabetically *)
val to_sorted_freq_list : t -> (Tag.t * int) list

val print_sorted_freq_list : t -> unit

(** Get all notes that fall under the tag *)
val find : t -> Tag.t -> Set.M(Note).t

val param : t Command.Param.t
