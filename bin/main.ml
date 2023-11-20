open Stlc.Parse
open Stlc.Eval

let () =
  print_string "> ";
  let line = read_line () in
  match Angstrom.parse_string ~consume:All exp line with
  | Ok e -> print_endline @@ string_of_exp (run e)
  | Error msg -> failwith msg
