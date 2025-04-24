open! Core

type t = Set.M(Note).t Map.M(Tag).t [@@deriving sexp_of]

let make notes =
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
;;

let to_sorted_freq_list t =
  Map.map t ~f:Set.length
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

let find t tag = Map.find t tag |> Option.value ~default:(Set.empty (module Note))

let param =
  let%map_open.Command base_path = Base_path.param in
  let notes = Note.load base_path |> ok_exn in
  make notes
;;

let test_note_list =
  [ ( "ocaml_notes"
    , {|ocaml, math

    this is some text for a note about ocaml and math|}
    )
  ; "work/cool_thing", "ocaml, plans"
  ; ( "random_note"
    , {|

    note text for a note with no tags|}
    )
  ]
;;

let test_notes = Note.Internal.make_all test_note_list Tag_dag.empty |> ok_exn

let%expect_test "make tag info" =
  print_s [%sexp (make test_notes : t)];
  [%expect
    {|
    ((math (((name ocaml_notes) (tags (math ocaml)))))
     (ocaml
      (((name ocaml_notes) (tags (math ocaml)))
       ((name work/cool_thing) (tags (ocaml plans)))))
     (plans (((name work/cool_thing) (tags (ocaml plans))))))
    |}]
;;

let%expect_test "list tags" =
  print_sorted_freq_list (make test_notes);
  [%expect
    {|
    tag (occurrences)
    ocaml (2)
    math (1)
    plans (1)
    |}]
;;

let%expect_test "search by tag" =
  let tag1, tag2 = Tag.of_string_exn "ocaml", Tag.of_string_exn "programming" in
  let tag_dag = Tag_dag.(add_edge empty ~from:tag1 ~to_:tag2 |> ok_exn) in
  let notes =
    Note.Internal.make_all (("python_notes", "programming") :: test_note_list) tag_dag
    |> ok_exn
  in
  let t = make notes in
  print_s [%sexp (find t tag2 : Set.M(Note).t)];
  [%expect
    {|
    (((name ocaml_notes) (tags (math ocaml programming)))
     ((name python_notes) (tags (programming)))
     ((name work/cool_thing) (tags (ocaml plans programming))))
    |}];
  print_s [%sexp (find t (Tag.of_string_exn "nonexistent") : Set.M(Note).t)];
  [%expect {| () |}]
;;
