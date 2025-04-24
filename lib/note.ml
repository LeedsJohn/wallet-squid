open! Core

module Note = struct
  module T = struct
    type t =
      { name : string
      ; tags : Set.M(Tag).t
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

include Note

let read_tags text =
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
  Ok (Set.of_list (module Tag) tags)
;;

let make_all notes =
  let%bind.Or_error note_list =
    List.map notes ~f:(fun (filename, content) ->
      let%bind.Or_error tags =
        read_tags content |> Or_error.tag_s ~tag:[%message "" ~name:(filename : string)]
      in
      Ok { name = filename; tags })
    |> Or_error.all
  in
  Ok (Set.of_list (module Note) note_list)
;;

let ls_recursive ~base_path =
  let rec aux acc cur =
    let names =
      Sys_unix.ls_dir cur |> List.map ~f:(fun fname -> [%string "%{cur}/%{fname}"])
    in
    let note_files =
      List.filter names ~f:(fun path ->
        let is_file =
          match Sys_unix.is_file path with
          | `Yes -> true
          | `No | `Unknown -> false
        in
        String.is_suffix path ~suffix:".md" && is_file)
    in
    let dirs =
      List.filter names ~f:(fun path ->
        match Sys_unix.is_directory path with
        | `Yes -> true
        | `No | `Unknown -> false)
    in
    List.fold dirs ~init:(acc @ note_files) ~f:aux
  in
  aux [] base_path
;;

let load base_path =
  let base_path = Base_path.to_filename base_path in
  ls_recursive ~base_path
  |> List.map ~f:(fun file_path ->
    let fname =
      String.chop_prefix_exn file_path ~prefix:(base_path ^ "/")
      |> String.chop_suffix_exn ~suffix:".md"
    in
    let content = In_channel.read_all file_path in
    fname, content)
  |> make_all
;;

module Internal = struct
  let make_all = make_all
end

let%expect_test "read_tags" =
  let good_line = "a, b, john_-023409" in
  print_s [%sexp (read_tags good_line : Set.M(Tag).t Or_error.t)];
  [%expect {| (Ok (a b john_-023409)) |}];
  let bad_line = "a, b, john_-023409, CAPITAL" in
  print_s [%sexp (read_tags bad_line : Set.M(Tag).t Or_error.t)];
  [%expect
    {|
    (Error
     ("tag must only contain lowercase letters, digits, dashes, and underscores"
      (tag CAPITAL)))
    |}]
;;

let%expect_test "load" =
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
  in
  print_s [%sexp (make_all good_notes : Set.M(Note).t Or_error.t)];
  [%expect
    {|
    (Ok
     (((name file1) (tags (tag1 tag2))) ((name file3) (tags ()))
      ((name subdir/file2) (tags (a tag1)))))
    |}];
  let bad_notes =
    [ "file1", "InvalidTag"; "file2", "good_tag"; "file3", "anotherBadTag" ]
  in
  print_s [%sexp (make_all bad_notes : Set.M(Note).t Or_error.t)];
  [%expect
    {|
    (Error
     (((name file1)
       ("tag must only contain lowercase letters, digits, dashes, and underscores"
        (tag InvalidTag)))
      ((name file3)
       ("tag must only contain lowercase letters, digits, dashes, and underscores"
        (tag anotherBadTag)))))
    |}]
;;
