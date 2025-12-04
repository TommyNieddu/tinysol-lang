(******************************************************************************)
(*                                    Tinysol CLI                             *)
(******************************************************************************)

open Ast
open Types
open Utils
open Main
open Prettyprint

let string_of_cli_cmd = function 
  | Faucet(a,n) -> "faucet " ^ a ^ " " ^ string_of_int n
  | Deploy(tx,filename) -> "deploy " ^ string_of_transaction tx ^ " " ^ filename
  | CallFun tx -> string_of_transaction tx
  | Revert tx -> "revert " ^ string_of_transaction tx
  | Assert(a,e) -> "assert " ^ a ^ " " ^ string_of_expr e 
  | SetBlockNum(n) -> "block.number = " ^ string_of_int n

let is_empty_or_comment (s : string) =
  let len = String.length s in
  (* skip leading spaces *)
  let rec skip i =
    if i >= len then true                      (* string is only spaces → empty *)
    else if s.[i] = ' ' then skip (i + 1)
    else if i + 1 < len && s.[i] = '/' && s.[i+1] = '/' then true
    else false
  in
  skip 0

let is_assert = function 
  | Assert(_) -> true
  | _ -> false

let exec_cli_cmd (cc : cli_cmd) (st : sysstate) : sysstate = match cc with
  | Faucet(a,n) -> faucet a n st
  | Deploy(tx,filename) -> 
      let src = filename |> read_file 
      in st |> deploy_contract tx src
  | CallFun tx -> st |> exec_tx 1000 tx
  | Revert tx -> (try 
      st |> exec_tx 1000 tx 
      |> fun _ -> failwith ("test failed: transaction " ^ string_of_transaction tx ^ " should revert") 
    with _ -> st)
  | Assert(a,e) -> 
    let env0 = bind "this" (Addr a) botenv in 
    (match eval_expr { st with stackenv = [env0] } e with
    | Bool true -> st
    | _ -> failwith ("assertion violation: " ^ string_of_cli_cmd cc)) 
  | SetBlockNum(n) -> { st with blocknum = n }

let exec_cli_cmd_list (verbose : bool) (ccl : cli_cmd list) (st : sysstate) = 
  List.fold_left 
  (fun sti cc -> 
    if verbose && not (is_assert cc) then 
      print_endline (string_of_sysstate [] sti ^ "\n--- " ^ string_of_cli_cmd cc ^ " --->")
    else ();  
    try 
      exec_cli_cmd cc sti
    with ex -> print_endline (string_of_sysstate [] sti); raise ex)
  st
  ccl
