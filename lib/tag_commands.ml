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
       let raw_note_content = Raw_note_content.load_all base_path in
       let tag_dag =
         Tag_dag.(
           load base_path raw_note_content |> ok_exn |> add_edge ~from ~to_ |> ok_exn)
       in
       Tag_dag.save tag_dag base_path)
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
       let raw_note_content = Raw_note_content.load_all base_path in
       let tag_dag =
         Tag_dag.(load base_path raw_note_content |> ok_exn |> remove_edge ~from ~to_)
       in
       Tag_dag.save tag_dag base_path)
;;

let search =
  Command.basic
    ~summary:"Find notes with given tag"
    (let%map_open.Command tag_info = Tag_info.param
     and tag = anon ("tag" %: Tag.arg_type) in
     fun () ->
       Tag_info.find tag_info tag
       |> Set.iter ~f:(fun { name; tags = _; content = _ } -> print_endline name))
;;

let make_graphviz =
  Command.basic
    ~summary:
      "Print graphviz DOT language representation of the tag\n\
      \    relationship graph to standard out"
    (let%map_open.Command base_path = Base_path.param in
     fun () ->
       let raw_note_content = Raw_note_content.load_all base_path in
       let tag_dag = Tag_dag.load base_path raw_note_content |> ok_exn in
       Tag_dag.print_dot tag_dag)
;;

let command =
  Command.group
    ~summary:"commands to interact with tags"
    [ "list", list_tags
    ; "add-connection", add_connection
    ; "remove-connection", remove_connection
    ; "search", search
    ; "make-graphviz", make_graphviz
    ]
;;
