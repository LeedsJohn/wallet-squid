open! Core

(* Surrounds string with spaces *)
let pad_string s ~n =
  let leftover = n - String.length s in
  let left = (leftover / 2) + (leftover % 2) in
  let right = leftover / 2 in
  String.init left ~f:(fun _ -> ' ') ^ s ^ String.init right ~f:(fun _ -> ' ')
;;

let print_exn ~headers ~content =
  let content = headers :: content in
  let content = List.map content ~f:Array.of_list |> Array.of_list in
  let width = Array.length content.(0) in
  let height = Array.length content in
  let col_max_lengths = Array.create ~len:width 0 in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      col_max_lengths.(x)
      <- Int.max col_max_lengths.(x) (String.length content.(y).(x) + 2)
    done
  done;
  let row_string ~start ~sep ~end_ f =
    let middle = List.range 0 width |> List.map ~f |> String.concat ~sep in
    start ^ middle ^ end_
  in
  let content_row_string y =
    row_string ~start:"│" ~sep:"│" ~end_:"│" (fun x ->
      pad_string content.(y).(x) ~n:col_max_lengths.(x))
  in
  let get_blank_word_f x =
    (* can't make a char out of the pipe characters so that's why I'm doing this *)
    List.init col_max_lengths.(x) ~f:(fun _ -> "─") |> String.concat
  in
  let top_row = row_string ~start:"┌" ~sep:"┬" ~end_:"┐" get_blank_word_f in
  let middle_row = row_string ~start:"├" ~sep:"┼" ~end_:"┤" get_blank_word_f in
  let bottom_row = row_string ~start:"└" ~sep:"┴" ~end_:"┘" get_blank_word_f in
  print_endline top_row;
  for y = 0 to height - 2 do
    print_endline (content_row_string y);
    print_endline middle_row
  done;
  print_endline (content_row_string (height - 1));
  print_endline bottom_row
;;

let%expect_test "print box" =
  let headers = [ "a"; "b"; "c" ] in
  let content = [ [ "a1"; "b1"; "c1" ]; [ "a2"; "b2"; "c2" ]; [ "a2"; "b2"; "c2" ] ] in
  print_exn ~headers ~content;
  [%expect
    {|
    ┌────┬────┬────┐
    │  a │  b │  c │
    ├────┼────┼────┤
    │ a1 │ b1 │ c1 │
    ├────┼────┼────┤
    │ a2 │ b2 │ c2 │
    ├────┼────┼────┤
    │ a2 │ b2 │ c2 │
    └────┴────┴────┘
    |}];
  let headers = [ "this is a longer header"; "bruh"; "c" ] in
  let content =
    [ [ "a1"; "b1"; "a" ]
    ; [ "a2"; "john john john john john"; "b" ]
    ; [ "a2"; "b2"; "c" ]
    ]
  in
  print_exn ~headers ~content;
  [%expect
    {|
    ┌─────────────────────────┬──────────────────────────┬───┐
    │ this is a longer header │           bruh           │ c │
    ├─────────────────────────┼──────────────────────────┼───┤
    │            a1           │            b1            │ a │
    ├─────────────────────────┼──────────────────────────┼───┤
    │            a2           │ john john john john john │ b │
    ├─────────────────────────┼──────────────────────────┼───┤
    │            a2           │            b2            │ c │
    └─────────────────────────┴──────────────────────────┴───┘
    |}]
;;
