%{
    open Ast
    open Location
    let mkl l = make_location l
    let mki name l = Ident {name; loc = mkl l}
    let mklvl name l = Lval (Var (Ident {name = name; loc = mkl l}))
    let mkvar name l = Var (Ident {name = name; loc = mkl l})
%}

// end of file
%token EOF
// string literals
%token <string> STRING_LIT   (* Strings quoted with "" or {}. *)
// integer literals
%token <int64> INT_LIT
// booleans
%token TRUE FALSE
// length operation; for arrays and strings
%token LENGTHOF
// arithmetic oprations
%token PLUS MINUS MUL DIV REM
// comparison operators
%token LT LE GT GE
// logical operations
%token LOR LAND LNOT
// equality
%token EQ NEQ
// assignment
%token ASSIGN
// punctuation
%token QUESTIONMARK COLON COMMA SEMICOLON
// accessors
%token DOT LBRACKET RBRACKET
// braces
%token LBRACE RBRACE
// parentheses
%token LPAREN RPAREN
// identifiers
%token <string> IDENT
// keywords
%token NIL VAR LET IF ELSE WHILE FOR BREAK CONTINUE RETURN NEW
// types
%token INT BOOL STRING BYTE VOID RECORD


%start <program> program_list

%left COMMA
%left ASSIGN 

%left LOR
%left LAND

%left RPAREN

%left NEQ 

%nonassoc EQ

%left PLUS MINUS 
%nonassoc LT LE GE GT
%left MUL DIV REM
%left UMINUS 
%right ELSE 

%%

%inline binop:
    MUL { Mul {loc = mkl $loc}}
    | DIV { Div {loc = mkl $loc}}
    | PLUS { Plus {loc = mkl $loc}}
    | MINUS { Minus {loc = mkl $loc}}
    | LE { Le {loc = mkl $loc }}
    | LT { Lt {loc = mkl $loc }}
    | LAND { Land {loc = mkl $loc }}
    | LOR { Lor {loc = mkl $loc }}
    | EQ { Eq {loc = mkl $loc }}
    | NEQ { NEq {loc = mkl $loc }}
    | REM { Rem { loc = mkl $loc }}
    | GE { Ge {loc = mkl $loc }}
    | GT { Gt {loc = mkl $loc }}

lval:
    l = IDENT {(Var (Ident {name = l; loc = mkl $loc}))}

%inline tp:
    BOOL { Bool {loc =  mkl $loc} }
    | INT { Int {loc = (make_location $loc)} }
    | VOID { Void {loc = mkl $loc} }
    | i = IDENT { Record {recordname = RecordName {name = i; loc = mkl $loc}}}
    | STRING { Str {loc = mkl $loc} }
    | t = array_tp { t }

array_tp:
    | LBRACKET t = tp RBRACKET { Array {typ = t; loc = mkl $loc}}
    | LBRACKET t = array_tp RBRACKET { Array {typ = t; loc = mkl $loc}}

%inline unoptp:
    | MINUS { Neg {loc = mkl $loc }}
    | LNOT { Lnot {loc = mkl $loc }}


arg_list:
    | expr COMMA arg_list { $3 @ [$1] }
    | { [] }
    | expr { [$1] }

expr:
    i = INT_LIT { Integer {int = i; loc = mkl $loc}}
    | l = lval {Lval l}
    | TRUE {Boolean { bool = true; loc = mkl $loc }}
    | FALSE {Boolean { bool = false; loc = mkl $loc }}
    (* UNARY OPERTORS BEFORE SINCE THEY HAVE HIGHER PRECEDENS *)
    | op = unoptp e = expr %prec UMINUS
        {UnOp {op = op; operand = e; loc = mkl $loc }}
    | op = unoptp LPAREN e = expr RPAREN
        {UnOp {op = op; operand = e; loc = mkl $loc }}

    (* BINOP EXPRESSIONS *)
    | left = expr op = binop right = expr
        {BinOp {op = op; left = left; right = right; loc = mkl $loc}}
    | expr op = binop LPAREN expr RPAREN
        {BinOp {op = op; left = $1; right = $4; loc = mkl $loc}}
    | LPAREN expr RPAREN op = binop expr 
        {BinOp {op = op; left = $2; right = $5; loc = mkl $loc}}
    | LPAREN expr RPAREN op = binop LPAREN expr RPAREN
        {BinOp {op = op; left = $2; right = $6; loc = mkl $loc}}
        
    (* New record (record creating?? ) *)
    | NEW name = IDENT LBRACE f = expfieldlist RBRACE { RcrdCreate {recordname = RecordName {name; loc = mkl $loc}; fields = f; loc = mkl $loc}}

    (* Array creation *)
    | NEW tp = tp LBRACKET e = expr RBRACKET { ArrayCreate {typ = tp; size = e; loc = mkl $loc}}
    (* Array access *)
    | e = expr LBRACKET i = expr RBRACKET { Lval (Idx {arr = e; index = i; loc = mkl $loc})}
    (* Array update *)
    | e1 = expr LBRACKET i = expr RBRACKET ASSIGN e2 = expr { Assignment {lvl = Idx {arr = e1; index = i; loc = mkl $loc}; rhs = e2; loc = mkl $loc}}
    (* Array update *)

    (* Length of array or string *)
    | LENGTHOF LBRACE e = expr RBRACE { LengthOf {str = e; loc = mkl $loc}}
    (* Nil *)

    (* ASSIGNMENT AND CALL *)
    | IDENT LPAREN arg_list RPAREN
        {Call {fname = mki $1 $loc; args = $3; loc = mkl $loc}}
    | IDENT ASSIGN expr { Assignment {lvl = mkvar $1 $loc; rhs = $3; loc = mkl $loc}}
    | IDENT ASSIGN LPAREN e = expr RPAREN { Assignment {lvl = mkvar $1 $loc; rhs =  e; loc = mkl $loc}}
    | e1 = expr DOT name = IDENT ASSIGN e2 = expr { Assignment {lvl = Fld {rcrd = e1; field = FieldName {name = name; loc = mkl $loc}; loc = mkl $loc}; rhs = e2; loc = mkl $loc}}
    (* Comma expr *)
    | left = expr COMMA right = expr { CommaExpr {left; right; loc = mkl $loc}}

    (* Field lookup in a record *)
    | e = expr DOT name = IDENT { Lval (Fld {rcrd = e; field = FieldName {name; loc = mkl $loc}; loc = mkl $loc})}

    // String literal:
    | STRING_LIT { StrLit {str = $1; loc = mkl $loc}}

    // Nil
    // | NIL { Nil {loc = mkl $loc}}


expfieldlist: 
    | { [] }
    | f = fieldexp SEMICOLON fl = expfieldlist { [f] @ fl }

fieldexp: 
    | name = IDENT ASSIGN e = expr { (FieldName {name; loc = mkl $loc}, e)}

stmt:
    RETURN ret = expr SEMICOLON {ReturnStm {ret = Some ret; loc = mkl $loc}}
    | RETURN LPAREN ret = expr RPAREN SEMICOLON {ReturnStm {ret = Some ret; loc = mkl $loc}}
    | RETURN SEMICOLON {ReturnStm {ret = None; loc = mkl $loc}}
    | VAR assignment_list SEMICOLON { VarDeclStm (DeclBlock {declarations = $2; loc = mkl $loc}) }

    | e = expr SEMICOLON {ExprStm {expr = Some (e); loc = mkl $loc}}
    | LPAREN e = expr RPAREN SEMICOLON {ExprStm {expr = Some (e); loc = mkl $loc}}
    | SEMICOLON { ExprStm {expr = None; loc = mkl $loc}}

    | LBRACE stmt_list RBRACE SEMICOLON { CompoundStm {stms = $2; loc = mkl $loc}} (* Online intepreter supports both*)
    | LBRACE stmt_list RBRACE { CompoundStm {stms = $2; loc = mkl $loc}}
    | IF LPAREN expr RPAREN stmt 
        {IfThenElseStm {cond = $3; thbr = $5; elbro = None; loc = mkl $loc}}
    | IF LPAREN expr RPAREN stmt ELSE stmt 
        {IfThenElseStm {cond = $3; thbr = $5; elbro = Some ($7); loc = mkl $loc}}
    | WHILE LPAREN expr RPAREN stmt
        {WhileStm {cond = $3; body = $5; loc = mkl $loc} }
    | BREAK SEMICOLON { BreakStm { loc = mkl $loc} }
    | CONTINUE SEMICOLON { ContinueStm { loc = mkl $loc} }
    | FOR LPAREN ini = option_init SEMICOLON ocond = option_expr SEMICOLON oupda = option_expr RPAREN bod = stmt
        {ForStm {init = ini; cond = ocond; update = oupda; body = bod; loc = mkl $loc}}

option_expr:
    | { None }
    | e = expr { Some (e) }

option_init:
    | { None }
    | VAR d = assignment_list { Some (FIDecl (DeclBlock {declarations = d; loc = mkl $loc}))}
    | e = expr { Some (FIExpr e)}

assignment_list:
    | assignment { [$1] }
    | assignment_list COMMA assignment { $1 @ [$3] }

assignment:
    | name = IDENT ASSIGN expr { Declaration {name = mki name $loc; tp = None; body = $3; loc = mkl $loc} }
    | name = IDENT ASSIGN LPAREN e = expr RPAREN { Declaration {name = mki name $loc; tp = None; body = e; loc = mkl $loc} }
    | name = IDENT COLON tp = tp  ASSIGN expr { Declaration {name = mki name $loc; tp = Some (tp); body = $5; loc = mkl $loc}}
    | name = IDENT COLON tp = tp  ASSIGN LPAREN e = expr RPAREN { Declaration {name = mki name $loc; tp = Some (tp); body = e; loc = mkl $loc}}


(* It is possible to use List std library menhir function here. But this was easier for us atm. *)
stmt_list:
    stmt {[$1]}
    | stmt stmt_list { $1 :: $2 }
    | { [] }

old_program_list:
    stmt {[$1]}
    | stmt stmt_list { $1 :: $2 }

param_list: 
    | param { [$1] }
    | { [] }
    | param_list COMMA param { $1 @ [$3] }

param:
    | name = IDENT COLON tp = tp { (mki name $loc, tp)}

fieldlist:
    | { [] }
    | f = field fi = fieldlist {f :: fi}

field: 
    | i = IDENT COLON t = tp SEMICOLON {
        Field {
                name = FieldName {name = i; loc = mkl $loc} 
            ;   typ = t
            ;   loc = mkl $loc
        }
    }

program: 
    | t = tp i = IDENT LPAREN p = param_list RPAREN LBRACE b = old_program_list RBRACE
        {Fun (Func {ret = t; name = mki i $loc; args = p; fbody = b; loc = mkl $loc})}
    | RECORD i = IDENT LBRACE f = fieldlist RBRACE {Rcr (Recor {
            recordname = RecordName {name = i; loc = mkl $loc}
        ;   fields = f
        })}

program_list:
    program EOF {[$1]}
    | program program_list { $1 :: $2 }