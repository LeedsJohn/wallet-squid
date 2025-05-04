open! Core

let fzf =
  Command.basic
    ~summary:"Fuzzy find notes"
    (let%map_open.Command base_path = Base_path.param in
     let notes = Note.load base_path |> ok_exn in
     fun () ->
       let note = Note.fzf notes in
       match note with
       | None -> print_endline "No note selected"
       | Some { name; content = _; tags = _ } ->
         let _ =
           Core_unix.exec
             ~prog:"nvim"
             ~argv:[ "nvim"; [%string "%{Base_path.to_filename base_path}/%{name}.md"] ]
             ()
         in
         ())
;;

let command =
  Command.group ~summary:"Wallet Squid" [ "tag", Tag_commands.command; "fzf", fzf ]
;;
