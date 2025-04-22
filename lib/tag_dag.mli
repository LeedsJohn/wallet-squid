open! Core

(** DAG (Directed acyclic graph) of tags. *)
type t [@@deriving sexp_of]

(** Returns map where the key is the connected tag and the value is the number of
    minimum number of edges to travel from the start tag to that end tag *)
val get_connected_tags : ?max_distance:int -> t -> Tag.t -> int Map.M(Tag).t

val load : Filename.t -> t Or_error.t

(** Error if the edge creates a cycle. Does nothing if the edge already exists *)
val add_edge : t -> from:Tag.t -> to_:Tag.t -> t Or_error.t

(** Does nothing if the edge does not exist *)
val remove_edge : t -> from:Tag.t -> to_:Tag.t -> t
