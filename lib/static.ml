open Ast

(* Types for expressions are a refinement of variable declaration types, 
 * since we want to give more specific types to integer constants in order
 * to have a smoother treatment of int and uint types 
 *)
type exprtype = 
  | BoolConstET of bool
  | BoolET
  | IntConstET of int
  | IntET
  | UintET
  | AddrET of bool
  | CustomET of ide
  | MapET of exprtype * exprtype

let rec string_of_exprtype = function
  | BoolConstET b   -> "bool " ^ (if b then "true" else "false")
  | BoolET          -> "bool"
  | IntConstET n    -> "int " ^ string_of_int n
  | IntET           -> "int"
  | UintET          -> "uint"
  | AddrET p        -> "address" ^ (if p then " payable" else "")
  | CustomET x      -> x
  | MapET(t1,t2)    -> string_of_exprtype t1 ^ " => " ^ string_of_exprtype t2

(* TypeError(expression, inferred type, expected type) *)
exception TypeError of expr * exprtype * exprtype
exception ImmutabilityError of ide
exception UndeclaredVar of ide
exception MultipleDecl of ide
exception EnumNameNotFound of ide
exception EnumOptionNotFound of ide * ide
exception EnumDupName of ide
exception EnumDupOption of ide * ide

let exprtype_of_decltype = function
  | IntBT  -> IntET
  | UintBT -> UintET
  | BoolBT -> BoolET
  | AddrBT(b)  -> AddrET(b)
  | CustomBT _ -> UintET (* TODO: be more precise *)

let lookup_type (x : ide) (vdl : var_decl list) : exprtype option =
  if x="msg.sender" then Some (AddrET false)
  else if x="msg.value" then Some UintET else 
  vdl 
  |> List.map (fun vd -> match vd with
    | VarT(t,_),x  -> (exprtype_of_decltype t),x 
    | MapT(tk,tv),x -> MapET(exprtype_of_decltype tk, exprtype_of_decltype tv),x)
  |> List.fold_left
  (fun acc (t,y) -> if acc=None && x=y then Some t else acc)
  None

let merge_var_decls old_vdl new_vdl = new_vdl @ old_vdl  

let rec dup = function 
  | [] -> None
  | x::l -> if List.mem x l then Some x else dup l

let no_dup_var_decls vdl = 
  vdl 
  |> List.map (fun vd -> match vd with (_,x) -> x) 
  |> dup
  |> fun res -> match res with None -> true | Some x -> raise (MultipleDecl x)  

let no_dup_fun_decls vdl = 
  vdl 
  |> List.map (fun fd -> match fd with 
    | Constr(_) -> "constructor"
    | Proc(f,_,_,_,_,_) -> f) 
  |> dup
  |> fun res -> match res with None -> true | Some x -> raise (MultipleDecl x)  

let subtype t0 t1 = match t1 with
  | BoolConstET _ -> (match t0 with BoolConstET _ -> true | _ -> false) 
  | BoolET -> (match t0 with BoolConstET _ | BoolET -> true | _ -> false) 
  | IntConstET _ -> (match t0 with IntConstET _ -> true | _ -> false)
  | UintET -> (match t0 with IntConstET n when n>=0 -> true | UintET -> true | _ -> false)
  | IntET -> (match t0 with IntConstET _ | IntET -> true | _ -> false) (* uint is not convertible to int *)
  | _ -> t0 = t1

let rec typecheck_expr (edl : enum_decl list) (vdl : var_decl list) = function
    True -> BoolConstET true
  | False -> BoolConstET false
  | IntConst n -> IntConstET n
  | AddrConst _ -> AddrET(false)
  | This -> AddrET(false) (* TODO: make more coherent with Solidity *)
  | BlockNum -> UintET
  | Var x -> (match lookup_type x vdl with
    | Some t -> t
    | None -> raise (UndeclaredVar x))
  | MapR(e1,e2) -> (match (typecheck_expr edl vdl e1, typecheck_expr edl vdl e2) with
    | MapET(t1k,t1v),t2 when t2 = t1k -> t1v 
    | MapET(t1k,_),t2 -> raise (TypeError (e2,t2,t1k))
    | _ -> failwith "TypeError: map" (* TODO: refine TypeError? *)
    ) 
  | BalanceOf(e) -> (match typecheck_expr edl vdl e with
        AddrET(_) -> UintET
      | _ as t -> raise (TypeError (e,t,AddrET(false))))
  | Not(e) -> (match typecheck_expr edl vdl e with
      | BoolConstET b -> BoolConstET (not b)
      | BoolET -> BoolET
      | _ as t -> raise (TypeError (e,t,BoolET)))
  | And(e1,e2) -> 
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (BoolConstET false,t2) when subtype t2 BoolET -> BoolConstET false
     | (t1,BoolConstET false) when subtype t1 BoolET -> BoolConstET false
     | (t1,t2) when subtype t1 BoolET && subtype t2 BoolET -> BoolET
     | (t,_) when not (subtype t BoolET) -> raise (TypeError (e1,t,BoolET))
     | (_,t) -> raise (TypeError (e2,t,BoolET)))
  | Or(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (BoolConstET true,t2) when subtype t2 BoolET -> BoolConstET true
     | (t1,BoolConstET true) when subtype t1 BoolET -> BoolConstET true
     | (t1,t2) when subtype t1 BoolET && subtype t2 BoolET -> BoolET
     | (t,_) when not (subtype t BoolET) -> raise (TypeError (e1,t,BoolET))
     | (_,t) -> raise (TypeError (e2,t,BoolET)))
  | Add(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> IntConstET (n1+n2)
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> UintET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> IntET
     | (t1,_) when not (subtype t1 IntET) -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | Mul(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> IntConstET (n1*n2)
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> UintET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> IntET
     | (t1,_) when not (subtype t1 IntET) -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | Sub(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> IntConstET (n1-n2)
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> UintET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> IntET
     | (t1,_) when not (subtype t1 IntET) -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | Eq(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> BoolConstET (n1 = n2)
     | (t1,t2) when t1=t2-> BoolET
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> BoolET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> BoolET
     | (t1,t2) -> raise (TypeError (e2,t2,t1)))
  | Neq(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> BoolConstET (n1 <> n2)
     | (t1,t2) when t1=t2-> BoolET
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> BoolET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> BoolET
     | (t1,t2) -> raise (TypeError (e2,t2,t1)))
  | Leq(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> BoolConstET (n1 <= n2)
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> BoolET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> BoolET
     | (t1,IntET) -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | Le(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> BoolConstET (n1 < n2)
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> BoolET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> BoolET
     | (t1,IntET) -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | Geq(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> BoolConstET (n1 >= n2)
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> BoolET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> BoolET
     | (t1,IntET) -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | Ge(e1,e2) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2) with
     | (IntConstET n1,IntConstET n2) -> BoolConstET (n1 > n2)
     | (t1,t2) when subtype t1 UintET && subtype t2 UintET -> BoolET
     | (t1,t2) when subtype t1 IntET && subtype t2 IntET -> BoolET
     | (t1,IntET) -> raise (TypeError (e1,t1,IntET))
     | (_,t2) -> raise (TypeError (e2,t2,IntET)))
  | IfE(e1,e2,e3) ->
    (match (typecheck_expr edl vdl e1,typecheck_expr edl vdl e2, typecheck_expr edl vdl e3) with
     | (BoolConstET true,t2,_) -> t2
     | (BoolConstET false,_,t3) -> t3
     | (BoolET,t2,t3) when subtype t2 t3 -> t3
     | (BoolET,t2,t3) when subtype t3 t2 -> t2
     | (BoolET,t2,t3) -> raise (TypeError (e3,t3,t2))
     | (t1,_,_) -> raise (TypeError (e1,t1,BoolET))
    )
  | IntCast(e) -> (match typecheck_expr edl vdl e with
      | IntConstET _ | IntET | UintET -> IntET
      | _ as t -> raise (TypeError (e,t,IntET)))
  | UintCast(e) -> (match typecheck_expr edl vdl e with
      | IntConstET n when n>=0 -> IntConstET n 
      | IntET | UintET -> UintET
      | _ as t -> raise (TypeError (e,t,IntET)))
  | AddrCast(e) -> (match typecheck_expr edl vdl e with
      | AddrET(b) -> AddrET(b)
      | IntConstET _ -> AddrET(false) 
      | UintET -> AddrET(false)
      | IntET  -> AddrET(false)
      | _ as t -> raise (TypeError (e,t,IntET))) 
  | PayableCast(e) -> (match typecheck_expr edl vdl e with
      | AddrET(_) -> AddrET(true)
      | IntConstET 0 -> AddrET(false)
      | _ as t -> raise (TypeError (e,t,IntET))) 
  | EnumOpt(enum_name,option_name) -> 
      edl
      |> List.filter (fun (Enum(y,_)) -> y=enum_name)
      |> fun edl -> (match edl with [Enum(_,ol)] -> Some ol | _ -> None)  
      |> fun l_opt -> (match l_opt with 
        | None -> raise (EnumNameNotFound enum_name)
        | Some ol -> (match List.find_index (fun o -> o=option_name) ol with
          None -> raise (EnumOptionNotFound(enum_name,option_name))
          | Some i -> IntConstET i))
  | EnumCast(x,e) -> (match typecheck_expr edl vdl e with
      | IntConstET _ | UintET | IntET -> CustomET x
      | _ as t -> raise (TypeError (e,t,IntET)))
  | FunCall(_) -> failwith "TODO"
  | ExecFunCall(_) -> failwith "TODO"

let is_immutable (x : ide) (vdl : var_decls) = 
  List.fold_left (fun acc vd -> match vd with
  | VarT(_,i),y when y=x -> i
  | _ -> acc  
  ) 
  false 
  vdl

let typecheck_local_decls (vdl : var_decls) = 
  List.for_all
  (fun vd -> match vd with 
    | MapT(_),_ -> false
    | _ -> true)
  vdl

let rec typecheck_cmd (is_constr : bool) (edl : enum_decl list) (vdl : var_decl list) = function 
    | Skip -> true
    | Assign(x,e) -> 
        (* the immutable modifier is not checked for the constructor *)
        if not is_constr && is_immutable x vdl then raise (ImmutabilityError x)
        else
        let te = typecheck_expr edl vdl e in
        let tx = typecheck_expr edl vdl (Var x) in
        if subtype te tx then true else raise (TypeError (e,te,tx))
    | MapW(x,ek,ev) ->  
        let tx = typecheck_expr edl vdl (Var x) in
        let tk = typecheck_expr edl vdl ek in
        let tv = typecheck_expr edl vdl ev in
        (match tx with
          | MapET(txk,_) when not (subtype tk txk) -> raise (TypeError (ek,tk,txk)) 
          | MapET(_,txv) when not (subtype tv txv) -> raise (TypeError (ev,tv,txv)) 
          | MapET(_,_) -> true
          | _ -> failwith ("Assignment to non-map variable " ^ x)
        )
    | Seq(c1,c2) -> 
        typecheck_cmd is_constr edl vdl c1 && 
        typecheck_cmd is_constr edl vdl c2
    | If(e,c1,c2) ->
        let te = typecheck_expr  edl vdl e in (match te with
          | BoolConstET true  -> typecheck_cmd is_constr edl vdl c1
          | BoolConstET false -> typecheck_cmd is_constr edl vdl c2
          | BoolET -> 
              typecheck_cmd is_constr edl vdl c1 && 
              typecheck_cmd is_constr edl vdl c2
          | _ -> raise (TypeError (e,te,BoolET)))
    | Send(ercv,eamt) -> 
        typecheck_expr edl vdl ercv = AddrET(true)  (* can only send to payable addresses *) 
        &&
        subtype (typecheck_expr edl vdl eamt) UintET
    | Req(e) -> 
        let te = typecheck_expr edl vdl e in
        if te = BoolET then true else raise (TypeError (e,te,BoolET))
    | Return(_) -> failwith "TODO"
    | Block(lvdl,c) -> 
        typecheck_local_decls lvdl &&
        let vdl' = merge_var_decls vdl lvdl in
        typecheck_cmd is_constr edl vdl' c
    | _ -> failwith "TODO (Call)"

let typecheck_fun (edl : enum_decl list) (vdl : var_decl list) = function
  | Constr (al,c,_) ->
      no_dup_var_decls al &&
      typecheck_local_decls al && 
      typecheck_cmd true edl (merge_var_decls vdl al) c
  | Proc (_,al,c,_,__,_) ->
      no_dup_var_decls al && 
      typecheck_local_decls al &&
      typecheck_cmd false edl (merge_var_decls vdl al) c

let rec dup_first (l : 'a list) : 'a option = match l with 
  | [] -> None
  | h::tl -> if List.mem h tl then Some h else dup_first tl

let typecheck_enums (edl : enum_decl list) = 
  match dup_first (List.map (fun (Enum(x,_)) -> x) edl) with
  | Some x -> raise (EnumDupName x)
  | None -> List.fold_left (fun _ (Enum(x,ol)) -> 
      match dup_first ol with 
      | Some o -> raise (EnumDupOption (x,o))
      | None -> true
    )
    true edl

let typecheck_contract (Contract(_,edl,vdl,fdl)) =
  typecheck_enums edl 
  &&
  (* no multiply declared variables *)
  no_dup_var_decls vdl
  &&
  (* no multiply declared functions *)
  no_dup_fun_decls fdl
  &&
  List.fold_left 
  (fun acc fd -> acc && typecheck_fun edl vdl fd)
  true
  fdl  
