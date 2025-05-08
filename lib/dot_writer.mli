open! Core

(** Bare minimum dot language printer. Example usage:

    [print_directed_graph_dot ~nodes:(Set.of_list ["a", "b"]) ~edges:[("a", "b")]]

    Output:
    digraph G {
        node0 [label="a"];
        node1 [label="b"];
        node0 -> node1;
    } *)
val print_directed_graph_dot
  :  nodes:Set.M(String).t
  -> edges:(string * string) list
  -> unit

val print_undirected_graph_dot
  :  nodes:Set.M(String).t
  -> edges:(string * string) list
  -> unit
