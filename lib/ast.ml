(* variable/function/contract identifier *)
type ide = string

(* address identifier *)
type addr = string

(* Abstract syntax of expressions *)

type expr =
  | BoolConst of bool
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
  | Lt of expr * expr           
  | Geq of expr * expr
  | Gt of expr * expr
  | IfE of expr * expr * expr           
  | IntCast of expr
  | UintCast of expr
  | AddrCast of expr
  | PayableCast of expr
  | EnumOpt of ide * ide
  | EnumCast of ide * expr
  | FunCall of expr * ide * expr * expr list
  | ExecFunCall of cmd
  
(* Abstract syntax of commands *)
          
and cmd =
  | Skip
  | Assign of ide * expr
  | MapW of ide * expr * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | Send of expr * expr         (* send(e1,e2) transfers e2 wei to e1 *)
  | Req of expr                 (* require(e) reverts if e is false *) 
  | Block of var_decl list * cmd
  | ExecBlock of cmd            (* Runtime only: c is the cmd being reduced *)
  | Decl of var_decl            (* Static-time only: Decl is converted into block*)
  | ProcCall of expr * ide * expr * expr list
  | ExecProc of cmd
  | Return of expr

(* Base types *)

and base_type = 
  | IntBT           (* int *)
  | UintBT          (* uint *) 
  | BoolBT          (* bool *)
  | AddrBT of bool  (* address (the bool b in AddrBT(b) tells if the address is payable (b=1) or not (b=0) *)
  | CustomBT of ide (* custom base type (contract or enum) *)

(* Variable types, consisting of:
  - a base type with a bool i telling whether the variable is mutable (i=false) or immutable (i=true)
  - a mapping from a base type to another base type
*)

and var_type = VarT of base_type * bool | MapT of base_type * base_type

(* a variable declaration (t,x) consists of:
   - a type t
   - an identifier x of the variable
 *)
and var_decl = var_type * ide 

(* Function visilibility modifiers *)

and visibility = 
  | Public 
  | Private

(* Function declarations
  - the constructor is always public, and it can be payable
  - functions can be either public or private, and they can be payable
 *)

and fun_decl =
  | Constr of var_decl list * cmd * bool (* payable *)
  | Proc of ide * var_decl list * cmd * visibility * bool * (base_type option) 

type enum_decl = Enum of (ide * ide list)

(* Contracts consist of:
 - a name (ide)
 - a list of enum declarations 
 - a list of variable declarations (contract state variables)
 - a list of function declarations 
 *)
type contract = Contract of ide * enum_decl list * var_decl list * fun_decl list


(******************************************************************************)
(*                                   Transactions                             *)
(******************************************************************************)

(* Exprval: values associated to (contract and local) variables *)

type exprval = 
  | Bool of bool 
  | Int of int
  | Addr of string
  | Map of (exprval -> exprval)

(* Transactions contain:
 - txsender: the address of the transaction sender (either an EOA or a contract address)
 - txto: the address of the called contract
 - txfun: the name of the called function. In a deploy transaction, the txfun is "constructor" and the first argument is the contract code
 - txvars: the list of actual parameters
 - txvalue: the amount of wei transferred from the sender to the caller along with the transaction 
 *)

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
