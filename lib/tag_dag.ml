open! Core

(* edges will be saved in [base_path]/[file_name] *)
let filename = "tag_edges.sexp"

type t = Set.M(Tag).t Map.M(Tag).t [@@deriving sexp]

let empty = Map.empty (module Tag)
let neighbors t tag = Map.find t tag |> Option.value ~default:(Set.empty (module Tag))

let get_connected_tags ?(max_distance = Int.max_value) t tag =
  let rec bfs res tags distance =
    let neighbors =
      Set.to_list tags
      |> List.map ~f:(neighbors t)
      |> Set.union_list (module Tag)
      |> Set.filter ~f:(fun tag -> not (Map.mem res tag))
    in
    let res =
      Map.merge_disjoint_exn res (Set.to_map neighbors ~f:(fun _tag -> distance))
    in
    if Set.length neighbors = 0 || distance = max_distance
    then res
    else bfs res neighbors (distance + 1)
  in
  bfs (Map.singleton (module Tag) tag 0) (Set.singleton (module Tag) tag) 1
;;

let get_path base_path = [%string "%{Base_path.to_filename base_path}/%{filename}"]
let save t base_path = Sexp.save (get_path base_path) (sexp_of_t t)

let used_tags base_path =
  let%bind.Or_error tags =
    Base_path.get_note_filenames base_path
    |> Set.to_list
    |> List.map ~f:(fun fname ->
      In_channel.read_lines fname
      |> List.hd
      |> Option.value ~default:""
      |> String.split ~on:','
      |> List.map ~f:String.strip
      |> List.filter ~f:(fun s -> String.length s > 0)
      |> List.map ~f:Tag.of_string
      |> List.map
           ~f:
             (Or_error.tag_s
                ~tag:
                  [%message
                    ""
                      ~name:
                        (String.chop_prefix_exn
                           fname
                           ~prefix:(Base_path.to_filename base_path ^ "/")
                         |> String.chop_suffix_exn ~suffix:".md"
                         : string)]))
    |> List.join
    |> Or_error.all
  in
  Ok (Set.of_list (module Tag) tags)
;;

(* This scans through every file in the directory again when it really doesn't need to.
   This might be something to fix in the future, but for the time being, I think that
   it is fine. *)
let load base_path =
  let path = get_path base_path in
  let t =
    match Sys_unix.is_file path with
    | `Yes -> Sexp.load_sexp path |> t_of_sexp
    | `No | `Unknown -> empty
  in
  let%bind.Or_error tags = used_tags base_path in
  Set.fold tags ~init:t ~f:(fun t tag ->
    Map.update t tag ~f:(function
      | None -> Set.empty (module Tag)
      | Some neighbors -> neighbors))
  |> Ok
;;

let find_cycle_starting_from_edge t ~from ~to_ =
  let rec dfs visited path =
    List.hd_exn path
    |> neighbors t
    |> Set.find_map ~f:(fun tag ->
      if Set.mem visited tag
      then Some (tag :: path)
      else dfs (Set.add visited tag) (tag :: path))
  in
  if Tag.equal from to_
  then Some [ from; to_ ]
  else (
    let%bind.Option path = dfs (Set.of_list (module Tag) [ to_; from ]) [ to_; from ] in
    Some (List.rev path))
;;

let add_edge t ~from ~to_ =
  let new_t =
    Map.update t from ~f:(function
      | None -> Set.singleton (module Tag) to_
      | Some s -> Set.add s to_)
  in
  match find_cycle_starting_from_edge new_t ~from ~to_ with
  | None -> Ok new_t
  | Some cycle ->
    Or_error.error_s
      [%message
        "New edge introduces cycle" (from : Tag.t) (to_ : Tag.t) (cycle : Tag.t list)]
;;

let remove_edge t ~from ~to_ =
  Map.update t from ~f:(function
    | None -> Set.empty (module Tag)
    | Some s -> Set.remove s to_)
;;

let%expect_test "adding edges" =
  let a, b, c = Tag.of_string_exn "a", Tag.of_string_exn "b", Tag.of_string_exn "c" in
  let tag_graph =
    add_edge empty ~from:a ~to_:b |> ok_exn |> add_edge ~from:b ~to_:c |> ok_exn
  in
  (* graph: a -> b -> c *)
  print_s [%sexp (tag_graph : t)];
  [%expect {| ((a (b)) (b (c))) |}];
  (* adding duplicate edge doesn't do anything *)
  let tag_graph = add_edge tag_graph ~from:a ~to_:b |> ok_exn in
  print_s [%sexp (tag_graph : t)];
  [%expect {| ((a (b)) (b (c))) |}];
  let tag_graph = add_edge tag_graph ~from:c ~to_:a in
  (* creates a cycle a -> b -> c -> a *)
  print_s [%sexp (tag_graph : t Or_error.t)];
  [%expect {| (Error ("New edge introduces cycle" (from c) (to_ a) (cycle (c a b c)))) |}];
  (* self loop *)
  let tag_graph = add_edge empty ~from:a ~to_:a in
  print_s [%sexp (tag_graph : t Or_error.t)];
  [%expect {| (Error ("New edge introduces cycle" (from a) (to_ a) (cycle (a a)))) |}]
;;

let%expect_test "get connected tags + removing" =
  let a, b, c, d =
    ( Tag.of_string_exn "a"
    , Tag.of_string_exn "b"
    , Tag.of_string_exn "c"
    , Tag.of_string_exn "d" )
  in
  let tag_graph =
    add_edge empty ~from:a ~to_:b
    |> ok_exn
    |> add_edge ~from:b ~to_:c
    |> ok_exn
    |> add_edge ~from:a ~to_:d
    |> ok_exn
  in
  (* graph: a -> b -> c
            V
            d *)
  print_s [%sexp (get_connected_tags tag_graph a : int Map.M(Tag).t)];
  [%expect {| ((a 0) (b 1) (c 2) (d 1)) |}];
  print_s [%sexp (get_connected_tags tag_graph a ~max_distance:1 : int Map.M(Tag).t)];
  [%expect {| ((a 0) (b 1) (d 1)) |}];
  let tag_graph = remove_edge tag_graph ~from:a ~to_:b in
  print_s [%sexp (get_connected_tags tag_graph a : int Map.M(Tag).t)];
  [%expect {| ((a 0) (d 1)) |}]
;;
