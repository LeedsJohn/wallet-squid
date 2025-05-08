open! Core

(** [filename] does not include the [Base_path.t]. *)
type t =
  { filename : Filename.t
  ; content : string
  }

(** Returns a [t] for every .md file in the base path (or a subdirectory of the base path). *)
val load_all : Base_path.t -> t list

(** Errors if the note contains any invalid tags. *)
val direct_tags : t -> Set.M(Tag).t Or_error.t
