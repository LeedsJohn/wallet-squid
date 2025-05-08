open! Core

type t =
  { filename : Filename.t
  ; content : string
  }

let load_all base_path =
  Base_path.get_note_filenames base_path
  |> Set.to_list
  |> List.map ~f:(fun fname ->
    let content = In_channel.read_all fname in
    let stripped_fname =
      String.chop_prefix_exn fname ~prefix:(Base_path.to_filename base_path ^ "/")
    in
    { filename = stripped_fname; content })
;;

let direct_tags { content; filename } =
  let%bind.Or_error tags =
    String.split_lines content
    |> List.hd
    |> Option.value ~default:""
    |> String.split ~on:','
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun s -> String.length s > 0)
    |> List.map ~f:Tag.of_string
    |> Or_error.all
    |> Or_error.tag_s ~tag:[%message "" (filename : Filename.t)]
  in
  Ok (Set.of_list (module Tag) tags)
;;

let%expect_test "direct_tags" =
  let good_line = "a, b, john_-023409" in
  print_s
    [%sexp
      (direct_tags { content = good_line; filename = "irrelevant" }
       : Set.M(Tag).t Or_error.t)];
  [%expect {| (Ok (a b john_-023409)) |}];
  let bad_line = "a, b, john_-023409, CAPITAL" in
  print_s
    [%sexp
      (direct_tags { content = bad_line; filename = "irrelevant" }
       : Set.M(Tag).t Or_error.t)];
  [%expect
    {|
    (Error
     ((filename irrelevant)
      ("tag must only contain lowercase letters, digits, dashes, and underscores"
       (tag CAPITAL))))
    |}]
;;
