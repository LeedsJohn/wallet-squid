open! Core

type t = Set.M(Note).t Map.M(Tag).t [@@deriving sexp_of]

let make notes =
  Set.to_list notes
  |> List.map ~f:(fun (({ tags; name = _; content = _ } : Note.t) as note) ->
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
  let headers = [ "tag"; "occurrences" ] in
  let content =
    to_sorted_freq_list t
    |> List.map ~f:(fun (tag, freq) -> [ Tag.to_string tag; Int.to_string freq ])
  in
  Box_printer.print_exn ~headers ~content
;;

let find t tag = Map.find t tag |> Option.value ~default:(Set.empty (module Note))

let param =
  let%map_open.Command notes = Note_set.param in
  make notes
;;

let raw_note_content =
  [ ( "ocaml_notes.md"
    , {|ocaml, math

    this is some text for a note about ocaml and math|}
    )
  ; "work/cool_thing.md", "ocaml, plans"
  ; ( "random_note.md"
    , {|

    note text for a note with no tags|}
    )
  ]
  |> List.map ~f:(fun (filename, content) -> { Raw_note_content.filename; content })
;;

let test_notes =
  List.map raw_note_content ~f:(fun raw_note_content ->
    Note.make raw_note_content Tag_dag.empty |> ok_exn)
  |> Set.of_list (module Note)
;;

let%expect_test "make tag info" =
  print_s [%sexp (make test_notes : t)];
  [%expect
    {|
    ((math
      (((name ocaml_notes) (tags (math ocaml))
        (content
          "ocaml, math\
         \n\
         \n    this is some text for a note about ocaml and math"))))
     (ocaml
      (((name ocaml_notes) (tags (math ocaml))
        (content
          "ocaml, math\
         \n\
         \n    this is some text for a note about ocaml and math"))
       ((name work/cool_thing) (tags (ocaml plans)) (content "ocaml, plans"))))
     (plans
      (((name work/cool_thing) (tags (ocaml plans)) (content "ocaml, plans")))))
    |}]
;;

let%expect_test "list tags" =
  print_sorted_freq_list (make test_notes);
  [%expect
    {|
    ┌───────┬─────────────┐
    │  tag  │ occurrences │
    ├───────┼─────────────┤
    │ ocaml │      2      │
    ├───────┼─────────────┤
    │  math │      1      │
    ├───────┼─────────────┤
    │ plans │      1      │
    └───────┴─────────────┘
    |}]
;;

let%expect_test "search by tag" =
  let tag1, tag2 = Tag.of_string_exn "ocaml", Tag.of_string_exn "programming" in
  let tag_dag = Tag_dag.(add_edge empty ~from:tag1 ~to_:tag2 |> ok_exn) in
  let notes =
    { Raw_note_content.filename = "python_notes.md"; content = "programming" }
    :: raw_note_content
    |> List.map ~f:(fun raw_note_content -> Note.make raw_note_content tag_dag)
    |> List.map ~f:ok_exn
    |> Set.of_list (module Note)
  in
  let t = make notes in
  print_s [%sexp (find t tag2 : Set.M(Note).t)];
  [%expect
    {|
    (((name ocaml_notes) (tags (math ocaml programming))
      (content
        "ocaml, math\
       \n\
       \n    this is some text for a note about ocaml and math"))
     ((name python_notes) (tags (programming)) (content programming))
     ((name work/cool_thing) (tags (ocaml plans programming))
      (content "ocaml, plans")))
    |}];
  print_s [%sexp (find t (Tag.of_string_exn "nonexistent") : Set.M(Note).t)];
  [%expect {| () |}]
;;
