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

let get_tags line =
  let%bind.Or_error tags =
    String.split line ~on:','
    |> List.map ~f:String.strip
    |> List.map ~f:Tag.of_string
    |> Or_error.all
  in
  Ok (Set.of_list (module Tag) tags)
;;

(* name must initially be prefixed base_path *)
let load_note ~base_path name =
  let%bind.Or_error tags =
    In_channel.read_lines name
    |> List.hd
    |> Option.value ~default:""
    |> get_tags
    |> Or_error.tag_s ~tag:[%message "" ~path:(name : string)]
  in
  Ok { name = String.chop_prefix_exn name ~prefix:(base_path ^ "/"); tags }
;;

let ls_recursive ~base_path =
  let rec aux acc cur =
    let names = Sys_unix.ls_dir cur |> List.map ~f:(fun fname -> cur ^ "/" ^ fname) in
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

(* tests in test/test_note.ml (because they depend on filesystem). This should
   potentially be restructured so that is not necessary. *)
let load ~base_path =
  let%bind.Or_error notes =
    ls_recursive ~base_path |> List.map ~f:(load_note ~base_path) |> Or_error.all
  in
  Ok (Set.of_list (module Note) notes)
;;

let%expect_test "get_tags" =
  let good_line = "a, b, john_-023409" in
  print_s [%sexp (get_tags good_line : Set.M(Tag).t Or_error.t)];
  [%expect {| (Ok (a b john_-023409)) |}];
  let bad_line = "a, b, john_-023409, CAPITAL" in
  print_s [%sexp (get_tags bad_line : Set.M(Tag).t Or_error.t)];
  [%expect
    {|
    (Error
     ("tag must only contain lowercase letters, digits, dashes, and underscores"
      (tag CAPITAL)))
    |}]
;;
