open! Core

(* TODO: potentially have a notes param that does all of this or something like that...? *)
let list_tags =
  Command.basic
    ~summary:"List all tags (ordered by frequency)"
    (let%map_open.Command base_path = Base_path.param in
     fun () ->
       let notes = Note.load base_path |> ok_exn in
       let tag_dag = Tag_dag.load base_path in
       Tag_info.(print_sorted_freq_list (make notes tag_dag)))
;;

let command =
  Command.group ~summary:"commands to interact with tags" [ "list", list_tags ]
;;
