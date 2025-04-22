open! Core

module T = struct
  type t = string [@@deriving compare, equal, sexp]
end

include T
include Comparator.Make (T)

let of_string s =
  if
    not
      (String.for_all s ~f:(fun c ->
         Char.is_lowercase c || Char.is_digit c || String.contains "-_" c))
  then
    error_s
      [%message
        "tag must only contain lowercase letters, digits, dashes, and underscores"
          ~tag:(s : string)]
  else if not (Int.between ~low:1 ~high:64 (String.length s))
  then error_s [%message "tag must be between 1 and 64 characters"]
  else Ok s
;;

let of_string_exn s = of_string s |> ok_exn
let to_string t = t

let%expect_test "tag from string" =
  print_s [%sexp (of_string "good_tag-123" : t Or_error.t)];
  [%expect {| (Ok good_tag-123) |}];
  print_s [%sexp (of_string "bad tag" : t Or_error.t)];
  [%expect
    {|
    (Error
     ("tag must only contain lowercase letters, digits, dashes, and underscores"
      (tag "bad tag")))
    |}];
  print_s [%sexp (of_string "badTag" : t Or_error.t)];
  [%expect
    {|
    (Error
     ("tag must only contain lowercase letters, digits, dashes, and underscores"
      (tag badTag)))
    |}];
  print_s
    [%sexp
      (of_string "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
       : t Or_error.t)];
  [%expect {| (Error "tag must be between 1 and 64 characters") |}];
  print_s [%sexp (of_string "" : t Or_error.t)];
  [%expect {| (Error "tag must be between 1 and 64 characters") |}]
;;
