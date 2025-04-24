open! Core

let list_tags =
  Command.basic
    ~summary:"List all tags (ordered by frequency)"
    (let%map_open.Command tag_info = Tag_info.param in
     fun () -> Tag_info.print_sorted_freq_list tag_info)
;;

let command =
  Command.group ~summary:"commands to interact with tags" [ "list", list_tags ]
;;
