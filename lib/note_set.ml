open! Core

type t = Set.M(Note).t [@@deriving sexp_of]

let make raw_content tag_dag =
  let%bind.Or_error notes =
    List.map raw_content ~f:(fun raw_content -> Note.make raw_content tag_dag)
    |> Or_error.all
  in
  Ok (Set.of_list (module Note) notes)
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
  let raw_content = Raw_note_content.load_all base_path in
  let tag_dag = Tag_dag.load base_path raw_content |> ok_exn in
  make raw_content tag_dag |> ok_exn
;;
