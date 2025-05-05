open! Core

(** Represents the name of the markdown editor.  For example, I use ["nvim"]. *)
type t = string

val param : t Command.Param.t
