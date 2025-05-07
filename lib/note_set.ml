open! Core

type t = Set.M(Note).t [@@deriving sexp_of]

let make notes tag_dag =
  let%bind.Or_error notes =
    List.map notes ~f:(fun (name, content) -> Note.make ~name ~content tag_dag)
    |> Or_error.all
  in
  Ok (Set.of_list (module Note) notes)
;;

let load base_path =
  let%bind.Or_error tag_dag = Tag_dag.load base_path in
  let notes =
    Base_path.get_note_filenames base_path
    |> Set.to_list
    |> List.map ~f:(fun fname ->
      ( String.chop_suffix_exn fname ~suffix:".md"
        |> String.chop_prefix_exn ~prefix:(Base_path.to_filename base_path ^ "/")
      , In_channel.read_all fname ))
  in
  make notes tag_dag
;;

let fzf notes =
  Set.to_list notes
  |> List.map ~f:(fun ({ Note.name; content; tags = _ } as note) ->
    [%string "%{name}\n%{content}"], note)
  |> Fzf.Pick_from.assoc
  |> Fzf.Blocking.pick_one ~preview:"echo {} | cat"
;;

let param =
  let%map_open.Command base_path = Base_path.param in
  load base_path |> ok_exn
;;

module For_testing = struct
  let make = make
end
