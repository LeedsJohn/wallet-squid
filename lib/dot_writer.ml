open! Core

let print_dot ~nodes ~edges =
  let nodes = Set.to_list nodes |> List.sort ~compare:String.compare in
  let edges =
    List.dedup_and_sort edges ~compare:(fun (s11, s12) (s21, s22) ->
      let open String in
      if s11 = s21 then compare s12 s22 else compare s11 s21)
  in
  print_endline "digraph G {";
  List.iter nodes ~f:(fun n -> print_endline (n ^ ";"));
  List.iter edges ~f:(fun (from, to_) -> print_endline [%string "%{from} -> %{to_};"]);
  print_endline "}"
;;

let%expect_test "simple dot" =
  let nodes = Set.of_list (module String) [ "a"; "b"; "c"; "d" ] in
  let edges = [ "a", "b"; "a", "c"; "c", "d"; "c", "d" ] in
  print_dot ~nodes ~edges;
  [%expect
    {|
    digraph G {
    a;
    b;
    c;
    d;
    a -> b;
    a -> c;
    c -> d;
    }
    |}]
;;
