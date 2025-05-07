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

let read_tags text tag_dag =
  let%bind.Or_error tags =
    String.split_lines text
    |> List.hd
    |> Option.value ~default:""
    |> String.split ~on:','
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun s -> String.length s > 0)
    |> List.map ~f:Tag.of_string
    |> Or_error.all
  in
  List.map tags ~f:(Tag_dag.get_connected_tags tag_dag)
  |> List.map ~f:Map.key_set
  |> Set.union_list (module Tag)
  |> Ok
;;

let make ~name ~content tag_dag =
  match read_tags content tag_dag with
  | Ok tags -> Ok { name; content; tags }
  | Error _ as e -> Or_error.tag_s e ~tag:[%message "" (name : string)]
;;

let%expect_test "read_tags" =
  let good_line = "a, b, john_-023409" in
  let tag_dag =
    Tag_dag.add_edge
      Tag_dag.empty
      ~from:(Tag.of_string_exn "a")
      ~to_:(Tag.of_string_exn "connected_tag")
    |> ok_exn
  in
  print_s [%sexp (read_tags good_line tag_dag : Set.M(Tag).t Or_error.t)];
  [%expect {| (Ok (a b connected_tag john_-023409)) |}];
  let bad_line = "a, b, john_-023409, CAPITAL" in
  print_s [%sexp (read_tags bad_line Tag_dag.empty : Set.M(Tag).t Or_error.t)];
  [%expect
    {|
    (Error
     ("tag must only contain lowercase letters, digits, dashes, and underscores"
      (tag CAPITAL)))
    |}]
;;

let%expect_test "make" =
  let good_notes =
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
    |> List.map ~f:(fun (name, content) -> make ~name ~content Tag_dag.empty)
    |> List.map ~f:(fun note -> [%sexp (note : t Or_error.t)])
  in
  List.iter good_notes ~f:print_s;
  [%expect
    {|
    (Ok
     ((name file1) (tags (tag1 tag2))
      (content  "tag1, tag2\
               \n\
               \n    this is some text for the note")))
    (Ok ((name subdir/file2) (tags (a tag1)) (content "tag1, a")))
    (Ok ((name file3) (tags ()) (content  "\
                                         \n\
                                         \n    note text")))
    |}];
  let bad_notes =
    [ "file1", "InvalidTag"; "file3", "anotherBadTag" ]
    |> List.map ~f:(fun (name, content) -> make ~name ~content Tag_dag.empty)
    |> List.map ~f:(fun note -> [%sexp (note : t Or_error.t)])
  in
  List.iter bad_notes ~f:print_s;
  [%expect
    {|
    (Error
     ((name file1)
      ("tag must only contain lowercase letters, digits, dashes, and underscores"
       (tag InvalidTag))))
    (Error
     ((name file3)
      ("tag must only contain lowercase letters, digits, dashes, and underscores"
       (tag anotherBadTag))))
    |}]
;;
