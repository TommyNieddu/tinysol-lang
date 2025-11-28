%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token NEQ
%token LEQ
%token LE
%token GEQ
%token GE
%token DOT
%token <string> ID
%token <string> CONST
%token <string> STRING
%token <string> ADDRLIT

%token SKIP
%token TAKES
%token ADDTAKES
%token SUBTAKES
%token CMDSEP
%token IF
%token ELSE
%token REQ

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LSQUARE
%token RSQUARE
%token MAPSTO
%token MAPPING
%token ENUM
%token EOF

%token CONTRACT
%token CONSTR
%token FUN
%token RETURN
%token INT
%token UINT
%token BOOL
%token ADDR
%token THIS
%token MSGSENDER
%token MSGVALUE
%token BALANCE
%token TRANSFER
%token COLON
%token QMARK
%token VALUE
%token ARGSEP
%token PUBLIC
%token PRIVATE
%token PAYABLE
%token IMMUTABLE
%token RETURNS 

%token FAUCET
%token DEPLOY
%token ASSERT
%token REVERT
%token BLOCKNUM

%token PRAGMA
%token SOLIDITY
%token CARET
%token <string> VERSION

%left OR
%left AND
%nonassoc NOT
%nonassoc EQ NEQ LEQ GEQ LE GE
%left PLUS MINUS
%nonassoc UMINUS
%left MUL

%left CMDSEP (* bart: check *)
%nonassoc ELSE

%start <contract> contract
%type <exprval> value
%type <var_decl> var_decl
%type <visibility> visibility
%type <fun_decl> fun_decl
%type <cmd> cmd
%type <var_decls> formal_args
%type <expr> expr 

%start <cmd> cmd_eof
%start <expr> expr_eof

%start <transaction> transaction
%start <cli_cmd> cli_cmd


%%

contract:
  | opt_pragma; CONTRACT; c=ID; LBRACE; el = list(enum_decl); vdl = contract_vars; fdl = list(fun_decl); RBRACE; EOF { Contract(c,el,vdl,fdl) }
;

rel_version:
  | GEQ {}
  | LEQ {}
  | TAKES {}

opt_pragma:
  | PRAGMA; SOLIDITY; rel_version; VERSION; CMDSEP { }
  | PRAGMA; SOLIDITY; CARET; VERSION; CMDSEP { }
  | /* empty */ { }

contract_vars:
  | vd = var_decl; CMDSEP; l = contract_vars { vd::l }
  | /* empty */ { [] }

enum_decl:
  | ENUM; x = ID; LBRACE; ol = separated_list(ARGSEP, ID); RBRACE { Enum(x,ol) }; 

actual_args:
  | a = separated_list(ARGSEP, value) { a } ;

value:
  | n = CONST { Int (int_of_string n) }
  | s = STRING { Addr s }
  | TRUE { Bool true }
  | FALSE { Bool false }

opt_value:
  | LBRACE; VALUE; COLON; e_value = expr; RBRACE; { e_value }
  | { IntConst 0 }

expr:
  | n = CONST { IntConst(int_of_string n) }
  | s = STRING { AddrConst(s) }
  | THIS { This }
  | MSGSENDER { Var("msg.sender") }
  | MSGVALUE { Var("msg.value") }
  | e = expr; DOT; BALANCE { BalanceOf(e) }
  | e = expr; DOT; o = ID { match e with Var(x) -> EnumOpt(x,o) | _ -> failwith "enum parser error"}
  | e1 = expr; LPAREN; e2 = expr; RPAREN { match e1 with Var(x) -> EnumCast(x,e2) | _ -> failwith "enum parser error"}
  | e_to = expr; DOT; f = ID; e_value = opt_value; LPAREN; e_args = separated_list(ARGSEP, expr); RPAREN { FunCall(e_to,f,e_value,e_args) }
  | TRUE { True }
  | FALSE { False }
  | BLOCKNUM { BlockNum }
  | NOT; e=expr { Not e }
  | MINUS; e=expr %prec UMINUS { Sub(IntConst 0, e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; NEQ; e2=expr { Neq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | e1=expr; LE; e2=expr { Le(e1,e2) }
  | e1=expr; GEQ; e2=expr { Geq(e1,e2) }
  | e1=expr; GE; e2=expr { Ge(e1,e2) }
  | e1=expr; LSQUARE; e2=expr; RSQUARE { MapR(e1,e2) }
  | LPAREN; e1 = expr; RPAREN; QMARK; e2 = expr; COLON; e3 = expr { IfE(e1,e2,e3) }
  | INT; LPAREN; e=expr; RPAREN; { IntCast(e) }
  | UINT; LPAREN; e=expr; RPAREN; { UintCast(e) }
  | ADDR; LPAREN; e=expr; RPAREN; { AddrCast(e) }
  | PAYABLE; LPAREN; e=expr; RPAREN; { PayableCast(e) }
  | x = ID { Var(x) }
  | LPAREN; e = expr; RPAREN { e }
;

expr_eof:
  | e = expr; EOF { e }
;

base_type:
  | INT  { IntBT  }
  | UINT { UintBT }
  | BOOL { BoolBT }
  | ADDR; p = opt_payable { AddrBT(p) }

opt_id:
  | ID { }
  | /* empty */ { }

opt_payable:
  | PAYABLE { true }
  | /* empty */ { false }

opt_immutable:
  | IMMUTABLE { true }
  | /* empty */ { false }

var_type:
  | t = base_type; i = opt_immutable { VarT(t,i) }
  | MAPPING; LPAREN; t1 = base_type; opt_id; MAPSTO; t2 = base_type; opt_id; RPAREN { MapT(t1,t2) }

var_decl:
  | t = var_type; x = ID { t,x }
  | t = ID; i = opt_immutable; x = ID { VarT(CustomBT(t),i),x }
;

visibility:
  | PUBLIC { Public }
  | PRIVATE { Private }
;

nonseq_cmd:
  | SKIP; CMDSEP;  { Skip }
  | REQ; e = expr; CMDSEP; { Req(e) } 
  | RETURN; e = expr; CMDSEP; { Return(e) } 
  | x = ID; TAKES; e = expr; CMDSEP; { Assign(x,e) }
  | x = ID; ADDTAKES; e = expr; CMDSEP; { Assign(x,Add(Var(x),e)) }
  | x = ID; SUBTAKES; e = expr; CMDSEP; { Assign(x,Sub(Var(x),e)) }
  | x = ID; LSQUARE; ek = expr; RSQUARE; TAKES; ev = expr; CMDSEP; { MapW(x,ek,ev) }
  | x = ID; LSQUARE; ek = expr; RSQUARE; ADDTAKES; ev = expr; CMDSEP; { MapW(x,ek,Add(MapR(Var x,ek),ev)) }
  | x = ID; LSQUARE; ek = expr; RSQUARE; SUBTAKES; ev = expr; CMDSEP; { MapW(x,ek,Sub(MapR(Var x,ek),ev)) }
  | rcv = expr; DOT; TRANSFER; LPAREN; amt=expr; RPAREN; CMDSEP; { Send(rcv,amt) }
  | vd = var_decl; CMDSEP; { Decl(vd) }
  | e_to = expr; DOT; f = ID; e_value = opt_value; LPAREN; e_args = separated_list(ARGSEP, expr); RPAREN; CMDSEP; { ProcCall(e_to,f,e_value,e_args) }
;

cmd:
  | c = nonseq_cmd { c }
  | c1 = cmd; c2 = cmd { Seq(c1,c2) }
  | IF LPAREN; e = expr; RPAREN; c1 = nonseq_cmd ELSE c2 = nonseq_cmd { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; ELSE c2 = nonseq_cmd { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; c1 = nonseq_cmd; ELSE LBRACE; c2 = cmd; RBRACE; { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; ELSE; LBRACE; c2 = cmd; RBRACE { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; c1 = nonseq_cmd { If(e,c1,Skip) }
  | LBRACE; c = cmd; RBRACE { Block([], c) }
;

cmd_eof:
  | c = cmd; EOF { c }
;

opt_returns:
  | RETURNS; LPAREN; t = base_type; RPAREN { Some t }
  | /* no return */ { None }

opt_cmd:
  | c = cmd { c}
  | { Skip }

fun_decl:
  /* constructor(al) payable? { c } */ 
  | CONSTR; LPAREN; al = formal_args; RPAREN; p = opt_payable; LBRACE; c = opt_cmd; RBRACE { Constr(al,c,p) }
  /* function f(al) [public|private]? payable? returns(r)? { c } */
  | FUN; f = ID; LPAREN; al = formal_args; RPAREN; v=visibility; p = opt_payable; r = opt_returns; LBRACE; c = opt_cmd; RBRACE { Proc(f,al,c,v,p,r) }
;

formal_args:
  | a = separated_list(ARGSEP, formal_arg) { a } ;

formal_arg:
  | INT;  x = ID { VarT(IntBT,false),x }
  | UINT; x = ID { VarT(UintBT,false),x }
  | BOOL; x = ID { VarT(BoolBT,false),x }
  | ADDR; p = opt_payable; x = ID { VarT(AddrBT(p),false),x }
;

opt_value_tx:
  | LBRACE; VALUE; COLON; n = CONST; RBRACE; { int_of_string n }
  | { 0 }

transaction:
  | s = ADDRLIT; COLON; c = ADDRLIT; v = opt_value_tx; LPAREN; al = actual_args; RPAREN 
  { { txsender = s;
      txto = c;
      txfun = "constructor";
      txargs = al;
      txvalue = v;
  } }
  | s = ADDRLIT; COLON; c = ADDRLIT; DOT; f = ID; v = opt_value_tx; LPAREN; al = actual_args; RPAREN 
  { { txsender = s;
      txto = c;
      txfun = f;
      txargs = al;
      txvalue = v;
  } }
;

cli_cmd:
  | tx = transaction { CallFun tx }
  | REVERT; tx = transaction { Revert tx }
  | FAUCET; a = ADDRLIT; n = CONST { Faucet(a, int_of_string n) }
  | DEPLOY; tx = transaction; filename = STRING { Deploy(tx,filename) }
  | ASSERT; a = ADDRLIT; e = expr_eof; { Assert(a,e) }
  | BLOCKNUM; TAKES; n = CONST; { SetBlockNum(int_of_string n) }
;