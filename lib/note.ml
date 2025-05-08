open! Core

module T = struct
  (* [tags] contains every tag in the note and all tags associated with that tag. For
       example, if a note is tagged with ocaml and the tag dag contains an edge from
       ocaml to programming, [tags] will contain both ocaml and programming. *)
  type t =
    { name : string
    ; tags : Set.M(Tag).t
    ; content : string
    }
  [@@deriving compare, sexp_of]
end

include T
include Comparator.Make (T)

let get_transitive_tags tags tag_dag =
  Set.to_list tags
  |> List.map ~f:(Tag_dag.get_connected_tags tag_dag)
  |> List.map ~f:Map.key_set
  |> Set.union_list (module Tag)
;;

let make ({ Raw_note_content.filename; content } as raw_note) tag_dag =
  let name = String.chop_suffix_exn filename ~suffix:".md" in
  let%bind.Or_error tags = Raw_note_content.direct_tags raw_note in
  let tags = get_transitive_tags tags tag_dag in
  Ok { name; content; tags }
;;

let%expect_test "get_transitive_tags" =
  let tags =
    [ "a"; "b"; "john_-023409" ]
    |> List.map ~f:Tag.of_string_exn
    |> Set.of_list (module Tag)
  in
  let tag_dag =
    Tag_dag.add_edge
      Tag_dag.empty
      ~from:(Tag.of_string_exn "a")
      ~to_:(Tag.of_string_exn "connected_tag")
    |> ok_exn
    |> Tag_dag.add_edge
         ~from:(Tag.of_string_exn "connected_tag")
         ~to_:(Tag.of_string_exn "another_tag")
    |> ok_exn
  in
  print_s [%sexp (get_transitive_tags tags tag_dag : Set.M(Tag).t)];
  [%expect {| (a another_tag b connected_tag john_-023409) |}]
;;

let%expect_test "make note" =
  let good_notes =
    [ ( "file1.md"
      , {|tag1, tag2

    this is some text for the note|}
      )
    ; "subdir/file2.md", "tag1, a"
    ; ( "file3.md"
      , {|

    note text|}
      )
    ]
    |> List.map ~f:(fun (filename, content) -> { Raw_note_content.filename; content })
    |> List.map ~f:(fun raw_note_content -> make raw_note_content Tag_dag.empty)
    |> List.map ~f:ok_exn
    |> List.map ~f:(fun note -> [%sexp (note : t)])
  in
  List.iter good_notes ~f:print_s;
  [%expect
    {|
    ((name file1) (tags (tag1 tag2))
     (content  "tag1, tag2\
              \n\
              \n    this is some text for the note"))
    ((name subdir/file2) (tags (a tag1)) (content "tag1, a"))
    ((name file3) (tags ()) (content  "\
                                     \n\
                                     \n    note text"))
    |}];
  let bad_notes =
    [ "file1.md", "InvalidTag"; "file3.md", "anotherBadTag" ]
    |> List.map ~f:(fun (filename, content) ->
      make { Raw_note_content.filename; content } Tag_dag.empty)
    |> List.map ~f:(fun note -> [%sexp (note : t Or_error.t)])
  in
  List.iter bad_notes ~f:print_s;
  [%expect
    {|
    (Error
     ((filename file1.md)
      ("tag must only contain lowercase letters, digits, dashes, and underscores"
       (tag InvalidTag))))
    (Error
     ((filename file3.md)
      ("tag must only contain lowercase letters, digits, dashes, and underscores"
       (tag anotherBadTag))))
    |}]
;;
