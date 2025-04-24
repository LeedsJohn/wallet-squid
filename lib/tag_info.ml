open! Core

type t =
  { tag_map : Set.M(Note).t Map.M(Tag).t
  ; dag : Tag_dag.t
  }
[@@deriving sexp_of]

let make notes dag =
  let tag_map =
    Set.to_list notes
    |> List.map ~f:(fun (({ tags; name = _ } : Note.t) as note) ->
      Set.to_list tags |> List.map ~f:(fun tag -> tag, note))
    |> List.join
    |> List.fold
         ~init:(Map.empty (module Tag))
         ~f:(fun acc (tag, note) ->
           Map.update acc tag ~f:(function
             | None -> Set.singleton (module Note) note
             | Some s -> Set.add s note))
  in
  { tag_map; dag }
;;

let to_sorted_freq_list { tag_map; dag = _ } =
  Map.map tag_map ~f:Set.length
  |> Map.to_alist
  |> List.sort ~compare:(fun (tag1, freq1) (tag2, freq2) ->
    if freq1 <> freq2 then Int.compare freq2 freq1 else Tag.compare tag1 tag2)
;;

let print_sorted_freq_list t =
  "tag (occurrences)"
  :: (to_sorted_freq_list t
      |> List.map ~f:(fun (tag, freq) -> [%string "%{Tag.to_string tag} (%{freq#Int})"]))
  |> String.concat ~sep:"\n"
  |> print_endline
;;

let find { tag_map; dag } tag =
  Tag_dag.get_connected_tags dag tag
  |> Map.keys
  |> List.map ~f:(Map.find tag_map)
  |> List.map ~f:(Option.value ~default:(Set.empty (module Note)))
  |> Set.union_list (module Note)
;;

let param =
  let%map_open.Command base_path = Base_path.param in
  let notes = Note.load base_path |> ok_exn in
  let tag_dag = Tag_dag.load base_path in
  make notes tag_dag
;;

let test_note_list =
  [ ( "file1"
    , {|tag1, tag2

    this is some text for the note|}
    )
  ; "subdir/file2", "tag1, a"
  ; ( "file3"
    , {|

    note text|}
    )
  ]
;;

let test_notes = Note.Internal.make_all test_note_list Tag_dag.empty |> ok_exn

let%expect_test "make tag info" =
  print_s [%sexp (make test_notes Tag_dag.empty : t)];
  [%expect
    {|
    ((tag_map
      ((a (((name subdir/file2) (tags (a tag1)))))
       (tag1
        (((name file1) (tags (tag1 tag2))) ((name subdir/file2) (tags (a tag1)))))
       (tag2 (((name file1) (tags (tag1 tag2)))))))
     (dag ()))
    |}]
;;

let%expect_test "list tags" =
  print_s
    [%sexp (to_sorted_freq_list (make test_notes Tag_dag.empty) : (Tag.t * int) list)];
  [%expect {| ((tag1 2) (a 1) (tag2 1)) |}]
;;

let%expect_test "search by tag" =
  let tag1, tag2 = Tag.of_string_exn "tag1", Tag.of_string_exn "z" in
  let notes =
    Note.Internal.make_all (("file4", "tag1, z") :: test_note_list) Tag_dag.empty
    |> ok_exn
  in
  let t = make notes Tag_dag.(add_edge empty ~from:tag1 ~to_:tag2 |> ok_exn) in
  print_s [%sexp (find t tag1 : Set.M(Note).t)];
  [%expect
    {|
    (((name file1) (tags (tag1 tag2))) ((name file4) (tags (tag1 z)))
     ((name subdir/file2) (tags (a tag1))))
    |}];
  print_s [%sexp (find t (Tag.of_string_exn "nonexistent") : Set.M(Note).t)];
  [%expect {| () |}]
;;
