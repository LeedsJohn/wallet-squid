open! Core

let list_tags =
  Command.basic
    ~summary:"List all tags (ordered by frequency)"
    (let%map_open.Command tag_info = Tag_info.param in
     fun () -> Tag_info.print_sorted_freq_list tag_info)
;;

let add_connection =
  Command.basic
    ~summary:"Add a connection from one tag to another"
    ~readme:(fun () ->
      "If you add a connection from tag x to tag y, then every note tagged with x will \
       also be tagged with y.")
    (let%map_open.Command base_path = Base_path.param
     and from =
       flag "-from" (required Tag.arg_type) ~doc:"tag Tag to add a connection from"
     and to_ = flag "-to" (required Tag.arg_type) ~doc:"tag Tag to add a connection to" in
     fun () ->
       let open Tag_dag in
       let tag_dag = load base_path |> add_edge ~from ~to_ |> ok_exn in
       save tag_dag base_path)
;;

let remove_connection =
  Command.basic
    ~summary:"Remove a connection from one tag to another"
    (let%map_open.Command base_path = Base_path.param
     and from =
       flag "-from" (required Tag.arg_type) ~doc:"tag Tag to remove a connection from"
     and to_ =
       flag "-to" (required Tag.arg_type) ~doc:"tag Tag to remove a connection to"
     in
     fun () ->
       let open Tag_dag in
       let tag_dag = load base_path |> remove_edge ~from ~to_ in
       save tag_dag base_path)
;;

let command =
  Command.group
    ~summary:"commands to interact with tags"
    [ "list", list_tags
    ; "add-connection", add_connection
    ; "remove-connection", remove_connection
    ]
;;
