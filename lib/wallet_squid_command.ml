open! Core

let fzf =
  Command.basic
    ~summary:"Fuzzy find notes"
    (let%map_open.Command base_path = Base_path.param
     and markdown_editor = Markdown_editor.param in
     let raw_note_content = Raw_note_content.load_all base_path in
     let tag_dag = Tag_dag.load base_path raw_note_content |> ok_exn in
     let notes = Note_set.make raw_note_content ~tag_dag |> ok_exn in
     fun () ->
       let note = Note_set.fzf notes in
       match note with
       | None -> print_endline "No note selected"
       | Some { name; content = _; tags = _ } ->
         let _ =
           Core_unix.exec
             ~prog:markdown_editor
             ~argv:
               [ markdown_editor
               ; [%string "%{Base_path.to_filename base_path}/%{name}.md"]
               ]
             ()
         in
         ())
;;

let make_graphviz =
  Command.basic
    ~summary:
      "Print graphviz DOT language representation of notes. Notes that share a tag are \
       connected."
    (let%map_open.Command tag_info = Tag_info.param in
     fun () -> Tag_info.print_dot tag_info)
;;

let command =
  Command.group
    ~summary:"Wallet Squid"
    [ "tag", Tag_commands.command; "fzf", fzf; "make-graphviz", make_graphviz ]
;;
