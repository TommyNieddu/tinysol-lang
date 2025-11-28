(* variable/function/contract identifier *)
type ide = string

(* address identifier *)
type addr = string

(* expressions *)

type expr =
  | True
  | False
  | IntConst of int
  | AddrConst of addr
  | BlockNum
  | This
  | Var of ide
  | MapR of expr * expr
  | BalanceOf of expr
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Leq of expr * expr
  | Le of expr * expr           
  | Geq of expr * expr
  | Ge of expr * expr
  | IfE of expr * expr * expr           
  | IntCast of expr
  | UintCast of expr
  | AddrCast of expr
  | PayableCast of expr
  | EnumOpt of ide * ide
  | EnumCast of ide * expr
  | FunCall of expr * ide * expr * expr list
  | ExecFunCall of cmd
  
(* commands *)
          
and cmd =
  | Skip
  | Assign of ide * expr
  | MapW of ide * expr * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | Send of expr * expr       (* send(e1,e2) transfers e2 wei to e1 *)
  | Req of expr               (* require(e) reverts if e is false *) 
  | Block of var_decls * cmd
  | ExecBlock of cmd          (* Runtime only: c is the cmd being reduced *)
  | Decl of var_decl          (* Static-time only: Decl is converted into block*)
  | ProcCall of expr * ide * expr * expr list
  | Return of expr

and base_type = 
  | IntBT           (* int *)
  | UintBT          (* uint *) 
  | BoolBT          (* bool *)
  | AddrBT of bool  (* address (the bool b in AddrBT(b) tells whether payable (b=1) or not (b=0) *)
  | CustomBT of ide (* custom base type (contract or enum) *)

(* a variable type can be either:
  - a base type with a bool i telling whether the variable is mutable (i=false) or immutable (i=true)
  - a mapping from a base type to another base type
*)

and var_type = VarT of base_type * bool | MapT of base_type * base_type

(* a variable declaration (t,x) consists of:
   - a type t
   - an identifier x of the variable
 *)
and var_decl = var_type * ide 

and visibility = 
  | Public 
  | Private

and fun_decl =
  | Constr of var_decls * cmd * bool (* payable *)
  | Proc of ide * var_decls * cmd * visibility * bool * (base_type option) 

and var_decls = var_decl list

and fun_decls = fun_decl list

type enum_decl = Enum of (ide * ide list)

type contract = Contract of ide * (enum_decl list) * var_decls * fun_decls

(******************************************************************************)
(*                                   Transactions                             *)
(******************************************************************************)

(* exprval: values associated to (contract and local) variables *)

type exprval = 
  | Bool of bool 
  | Int of int
  | Addr of string
  | Map of (exprval -> exprval)

(* in a deploy transaction, the txfun is "constructor" and the first argument is the contract code *)

type transaction = {
  txsender : addr;
  txto : addr;
  txfun : ide;
  txargs : exprval list;
  txvalue : int;
}

(******************************************************************************)
(*                                    Tinysol CLI                             *)
(******************************************************************************)

type cli_cmd = 
  | Faucet of addr * int
  | Deploy of transaction * string
  | CallFun of transaction
  | Revert of transaction
  | Assert of addr * expr
  | SetBlockNum of int
