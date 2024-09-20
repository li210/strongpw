open Parser
open Ast
open Parser
open Ast

let file = if Array.length Sys.argv > 1 then Sys.argv.(1) else "bin/test"
let read_f = open_in file

let read_f = open_in file

let () =
  let lexbuf = Lexing.from_channel read_f in
  let rec loop () =
    try
      (* let res = *) Parser.pwdf Lexer.token lexbuf;  (* in print_string (Ast.pw_string res); print_newline () *)
      loop ()
    with
    | Lexer.Eof -> () (* Continue processing *)
  in
  loop ()