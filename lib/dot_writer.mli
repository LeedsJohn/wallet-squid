open! Core

(** Bare minimum dot language printer. Example usage:

    [make_dot ~nodes:(Set.of_list ["a", "b"]) ~edges:[("a", "b")]]

    Output:
    digraph G {
        a;
        b;
        a -> b;
    } *)
val print_dot : nodes:Set.M(String).t -> edges:(string * string) list -> unit
