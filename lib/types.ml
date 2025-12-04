open Ast
open Utils

let is_val = function
  | BoolConst _ 
  | IntConst _
  | AddrConst _ -> true
  | _ -> false

let exprval_of_expr = function
  | BoolConst b -> (Bool b)
  | IntConst n  -> (Int n)
  | AddrConst s -> (Addr s)
  | _ -> failwith ("expression is not a value")

let int_of_expr e = match e with 
  | IntConst n  -> n
  | _ -> failwith "IntConst was expected"

let bool_of_expr e = match e with 
  | BoolConst b -> b
  | _  -> failwith "True or False was expected"

let addr_of_expr e = match e with 
  | AddrConst a -> a
  | _ -> failwith "AddrConst was expected"

let expr_of_exprval = function
  | Bool b -> BoolConst b
  | Int n -> IntConst n
  | Addr b -> AddrConst b
  | Map _ -> failwith "step_expr: wrong type checking of map?"

let addr_of_exprval v = match v with 
  | Addr a -> a
  | Bool _ -> failwith "value has type Bool but an Addr was expected"
  | Int _ -> failwith "value has type Int but an Addr was expected"
  | Map _ -> failwith "value has type Map but an Addr was expected"


(* local environment: 
    maps local variables, plus sender and value
    this is a transient storage, not preserved between calls
*)
type env = ide -> exprval

(* contract state: persistent contract storage, preserved between calls *)
type account_state = {
  balance : int;
  storage : ide -> exprval;
  code : contract option;
}

type sysstate = {
  accounts: addr -> account_state;
  stackenv: env list;
  blocknum: int;
  active: addr list; (* set of all active addresses (for debugging)*)
}

(* execution state of a command *)
type exec_state = 
  | St of sysstate 
  | CmdSt of cmd * sysstate
  | Reverted

let rec last_sysstate = function
    [] -> failwith "last on empty list"
  | [St st] -> st
  | _::l -> last_sysstate l


(* Functions to access and manipulate the state *)

let topenv (st: sysstate) : env = match st.stackenv with
  [] -> failwith "empty stack"
| e::_ -> e

let popenv (st: sysstate) : sysstate = match st.stackenv with
    [] -> failwith "empty stack"
  | _::el -> { st with stackenv = el } 

(* initial (empty) environment *)
let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
    
let bind x v f = fun y -> if y=x then v else f y

(* lookup for variable x in sysstate st *)

let lookup_env (x : ide) (el : env list) : exprval option =
  List.fold_left
  (fun acc e -> match acc with
    | Some v -> Some v
    | None -> try Some (e x) with _ -> None)
  None
  el

let lookup_var (x : ide) (st : sysstate) : exprval =
  (* look up for x in environment stack *)
  match lookup_env x st.stackenv with
  | Some v -> v 
  | None -> 
    (* retrieve the address of the currently executed contract *)
    match lookup_env "this" st.stackenv with
    | Some (Addr a) -> 
      (* look up for x in storage of a *)
      let cs = st.accounts a in
      cs.storage x
    | _ -> failwith "this not bound or not bound to an address"

let lookup_balance (a : addr) (st : sysstate) : int =
  try (st.accounts a).balance
  with _ -> 0

let lookup_enum_option (st : sysstate) (enum_name : ide) (option_name : ide) : int option = 
  try 
    let a = addr_of_exprval (Option.get(lookup_env "this" st.stackenv)) in 
    match (st.accounts a).code with
    | Some(Contract(_,edl,_,_)) -> 
      edl
      |> List.filter (fun (Enum(y,_)) -> y=enum_name)
      |> fun edl -> (match edl with [Enum(_,ol)] -> Some ol | _ -> None)  
      |> fun l_opt -> (match l_opt with 
        | None -> None
        | Some ol -> find_index (fun o -> o=option_name) ol)
    | _ -> assert(false) (* should not happen *)
  with _ -> None

let reverse_lookup_enum_option (st : sysstate) (enum_name : ide) (option_index : int) : ide option = 
  try
    let a = addr_of_exprval (Option.get(lookup_env "this" st.stackenv)) in 
    match (st.accounts a).code with
    | Some(Contract(_,edl,_,_)) -> 
      edl
      |> List.filter (fun (Enum(y,_)) -> y=enum_name)
      |> fun edl -> (match edl with [Enum(_,ol)] -> Some ol | _ -> None)  
      |> fun l_opt -> (match l_opt with 
        | None -> None
        | Some ol -> List.nth_opt ol option_index)
    | _ -> assert(false) (* should not happen *)
  with _ -> None


let exists_account (st : sysstate) (a : addr) : bool =
  try let _ = st.accounts a in true
  with _ -> false

let exists_ide_in_storage (cs : account_state) (x : ide) : bool = 
  try let _ = cs.storage x in true
  with _ -> false

(* 
  Updates the variable x to value x in environment stack el. 
  The variable is searched throughout the environment frames in the stack. 
 *)
let rec update_env (el : env list) (x : ide) (v : exprval) : env list =
 match el with
  | [] -> failwith (x ^ " not bound in env")
  | e::el' -> 
    try let _ = e x in (* checks if ide x is bound in e *)
      (bind x v e) :: el'
    with _ -> e :: (update_env el' x v)

(* 
  Updates the variable x to value x in state st. 
  The variable is first searched in the topmost environment, and then in the contract storage. 
 *)
let update_var (st : sysstate) (x : ide) (v : exprval) : sysstate = 
  (* first tries to update environment if x is bound there *)
   try 
    let el' = update_env st.stackenv x v in
    { st with stackenv = el' }
  with _ -> 
    (* if not, tries to update storage of a *)
    (* first, retrieve the address of the currently executed contract *)
    match lookup_env "this" st.stackenv with
    | Some (Addr a) -> 
      let cs = st.accounts a in
      if exists_ide_in_storage cs x then 
        let cs' = { cs with storage = bind x v cs.storage } in 
        { st with accounts = bind a cs' st.accounts }
      else failwith (x ^ " not bound in storage of " ^ a)   
    | _ -> failwith "this not bound or not bound to an address"


let update_map (st : sysstate) (x:ide) (k:exprval) (v:exprval) : sysstate = 
  let a = addr_of_exprval (Option.get(lookup_env "this" st.stackenv)) in 
  let cs = st.accounts a in
    if exists_ide_in_storage cs x then 
      match cs.storage x with
      | Map m ->
        let m' = bind k v m in
        let cs' = { cs with storage = bind x (Map m') cs.storage } in 
        { st with accounts = bind a cs' st.accounts }
      | _ -> failwith ("update_map: " ^ x ^ " is not a mapping")
      else failwith ("mapping " ^ x ^ " not bound in storage of " ^ a)   

