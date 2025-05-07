open! Core

type t =
  { name : string
  ; tags : Set.M(Tag).t
  ; content : string
  }
[@@deriving compare, sexp_of]

include Comparator.S with type t := t

val make : name:string -> content:string -> Tag_dag.t -> t Or_error.t
