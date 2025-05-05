open! Core

(** Errors if headers is a different length than every row in content or if any row in
    content is a different length from all the others. *)
val print_exn : headers:string list -> content:string list list -> unit
