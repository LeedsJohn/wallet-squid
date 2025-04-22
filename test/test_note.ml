open! Core
open! Wallet_squid

(* TODO: can / should this be written without using the file system? *)
let%expect_test "load notes" =
  let good_notes = Note.load ~base_path:"./example_notes_for_testing" in
  print_s [%sexp (good_notes : Set.M(Note).t Or_error.t)];
  [%expect
    {|
    (Ok
     (((name note1.md) (tags (a b c)))
      ((name subdir/note2.md) (tags (a b othertag)))))
    |}];
  let bad_notes = Note.load ~base_path:"./invalid_tag_notes" in
  print_s [%sexp (bad_notes : Set.M(Note).t Or_error.t)];
  [%expect
    {|
    (Error
     ((path ./invalid_tag_notes/bad_note.md)
      ("tag must only contain lowercase letters, digits, dashes, and underscores"
       (tag INVALID))))
    |}]
;;
