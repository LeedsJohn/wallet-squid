open! Core
open! Wallet_squid

(* TODO: can / should this be written without using the file system? *)
let%expect_test "load notes" =
  let base_path = Base_path.of_string "./example_notes_for_testing" |> ok_exn in
  let good_notes = Note_set.load base_path in
  print_s [%sexp (good_notes : Set.M(Note).t Or_error.t)];
  [%expect
    {|
    (Ok
     (((name note1) (tags (a b c))
       (content  "a, b, c\
                \n\
                \nThis is an example note for testing\
                \n"))
      ((name subdir/note2) (tags (a b othertag))
       (content
         "a, b, othertag\
        \n\
        \n# Note 2\
        \n\
        \nHere's another note for testing\
        \n"))))
    |}];
  let base_path = Base_path.of_string "./invalid_tag_notes" |> ok_exn in
  let bad_notes = Note_set.load base_path in
  print_s [%sexp (bad_notes : Set.M(Note).t Or_error.t)];
  [%expect
    {|
    (Error
     ((name bad_note)
      ("tag must only contain lowercase letters, digits, dashes, and underscores"
       (tag INVALID))))
    |}]
;;
