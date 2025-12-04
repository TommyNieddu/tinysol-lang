open Ast

(******************************************************************************)
(*                                   List utilities                           *)
(******************************************************************************)

let rec last = function
    [] -> failwith "last on empty list"
  | [st] -> st
  | _::l -> last l

let find_index f l =  
  let rec find_index_helper (b,i) f = function 
    [] -> (b,i)
  | x::l -> if b then (b,i)
            else if f x then (true,i) 
            else find_index_helper (b,i+1) f l 
  in 
    let (b,i) = find_index_helper (false,0) f l in
    if b then Some i else None

(******************************************************************************)
(*                                   File utilities                           *)
(******************************************************************************)

(* read file, and output it to a string *)

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

(* read line from standard input, and output it to a string *)

let read_line () =
  try Some(read_line())
  with End_of_file -> None
;;

let read_lines filename =
  let chan = open_in filename in
  let rec loop acc =
    match input_line chan with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in chan;
        List.rev acc
  in
  loop []

(******************************************************************************)
(*                                   Parsing utilities                        *)
(******************************************************************************)

let parse_expr (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expr_eof Lexer.read_token lexbuf in
  ast

let parse_cmd (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cmd_eof Lexer.read_token lexbuf in
  ast

let parse_contract (s : string) : contract =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.contract Lexer.read_token lexbuf in
  ast

let parse_transaction (s : string) : transaction =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.transaction Lexer.read_token lexbuf in
  ast

let parse_cli_cmd (s : string) : cli_cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cli_cmd Lexer.read_token lexbuf in
  ast
