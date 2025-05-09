open! Core

let process_nodes nodes (edges : (string * string) list) =
  (* make sure that all the nodes in [edges] are in nodes *)
  let edge_nodes =
    List.map edges ~f:(fun (a, b) -> Set.of_list (module String) [ a; b ])
    |> Set.union_list (module String)
  in
  let nodes =
    Set.union edge_nodes nodes |> Set.to_list |> List.sort ~compare:String.compare
  in
  let name_node_pairs =
    List.mapi nodes ~f:(fun i name -> name, [%string "node%{i#Int}"])
  in
  Map.of_alist_exn (module String) name_node_pairs
;;

let process_edges edges name_to_node =
  List.dedup_and_sort edges ~compare:(fun (s11, s12) (s21, s22) ->
    let open String in
    if s11 = s21 then compare s12 s22 else compare s11 s21)
  |> List.map ~f:(fun (a, b) -> Map.find_exn name_to_node a, Map.find_exn name_to_node b)
;;

let print_graph ~graph_type ~arrow_string ~nodes ~edges =
  (* nodes cannot start with a number but some files do, so I'm assigning each one a
     name and then setting the name with the label. *)
  let name_to_node = process_nodes nodes edges in
  let edges = process_edges edges name_to_node in
  print_endline [%string "%{graph_type} G {"];
  Map.iteri name_to_node ~f:(fun ~key:name ~data:node ->
    print_endline [%string {|%{"\t"}%{node} [label="%{name}"];|}]);
  List.iter edges ~f:(fun (from, to_) ->
    print_endline [%string "\t%{from} %{arrow_string} %{to_};"]);
  print_endline "}"
;;

let print_directed_graph_dot ~nodes ~edges =
  print_graph ~graph_type:"digraph" ~arrow_string:"->" ~nodes ~edges
;;

let print_undirected_graph_dot ~nodes ~edges =
  print_graph ~graph_type:"graph" ~arrow_string:"--" ~nodes ~edges
;;

let%expect_test "simple dot" =
  let nodes = Set.of_list (module String) [ "a"; "b"; "c"; "d" ] in
  let edges = [ "a", "b"; "a", "c"; "c", "d"; "c", "d" ] in
  print_directed_graph_dot ~nodes ~edges;
  [%expect
    {|
    digraph G {
    node0 [label="a"];
    node1 [label="b"];
    node2 [label="c"];
    node3 [label="d"];
    node0 -> node1;
    node0 -> node2;
    node2 -> node3;
    }
    |}]
;;
