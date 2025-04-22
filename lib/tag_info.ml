open! Core

type t = Set.M(Note).t Map.M(Tag).t [@@deriving sexp_of]

let make (notes : Set.M(Note).t) : t Or_error.t =
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
  |> Ok
;;

let to_sorted_freq_list t =
  Map.map t ~f:Set.length
  |> Map.to_alist
  |> List.sort ~compare:(fun (tag1, freq1) (tag2, freq2) ->
    if freq1 <> freq2 then Int.compare freq2 freq1 else Tag.compare tag1 tag2)
;;

(* TODO: this will eventually need to consider the hierarchical structure of tags *)
let find t tag = Map.find t tag |> Option.value ~default:(Set.empty (module Note))

let test_notes =
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
  |> Note.Internal.make_all
  |> ok_exn
;;

let%expect_test "make tag info" =
  print_s [%sexp (make test_notes : t Or_error.t)];
  [%expect
    {|
    (Ok
     ((a (((name subdir/file2) (tags (a tag1)))))
      (tag1
       (((name file1) (tags (tag1 tag2))) ((name subdir/file2) (tags (a tag1)))))
      (tag2 (((name file1) (tags (tag1 tag2)))))))
    |}]
;;

let%expect_test "list tags" =
  print_s [%sexp (to_sorted_freq_list (ok_exn (make test_notes)) : (Tag.t * int) list)];
  [%expect {| ((tag1 2) (a 1) (tag2 1)) |}]
;;

let%expect_test "search by tag" =
  let t = make test_notes |> ok_exn in
  print_s [%sexp (find t (Tag.of_string_exn "tag1") : Set.M(Note).t)];
  [%expect
    {| (((name file1) (tags (tag1 tag2))) ((name subdir/file2) (tags (a tag1)))) |}];
  print_s [%sexp (find t (Tag.of_string_exn "nonexistent") : Set.M(Note).t)];
  [%expect {| () |}]
;;
