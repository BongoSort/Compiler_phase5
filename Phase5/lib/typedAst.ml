(* -- Use this in your solution without modifications *)
module Sym = Symbol

type ident = Ident of {sym : Sym.symbol}
type fieldname = FieldName of { name : string}
type recordname = RecordName of {name : string}

type typ = | Void | Int | Bool | ErrorType | Record of {rname: recordname} | Array of {tp : typ} | String

type binop = | Plus | Minus | Mul | Div | Rem | Lt 
  | Le | Gt | Ge | Lor | Land | Eq | NEq

type unop = | Neg | Lnot

type expr =
| Integer of {int : int64}
| Boolean of {bool : bool}
| BinOp of {left : expr; op : binop; right : expr; tp : typ}
| UnOp of {op : unop; operand : expr; tp : typ}
| Lval of lval
| Assignment of {lvl : lval; rhs : expr; tp : typ}
| CommaExpr of { left : expr; right :expr; tp : typ}
| Call of {fname : ident; args : expr list; tp : typ}
| RcrdCreate of {recordname : recordname; fields : (fieldname * expr) list}
| StrLit of {str : string}
| ArrayCreate of {tp : typ; size : expr}
| LengthOf of {str : expr}
and lval =
| Var of {ident  : ident; tp : typ}
| Idx of { arr   : expr
         ; index : expr 
         ; tp    : typ
         }
| Fld of { rcrd     : expr 
         ; field    : fieldname
         ; rcrdname : string
         ; tp       : typ
         }

type single_declaration = Declaration of {name : ident; tp : typ; body : expr}

type declaration_block = DeclBlock of single_declaration list

type for_init =
| FIDecl of declaration_block
| FIExpr of expr

type statement =
| VarDeclStm of declaration_block
| ExprStm of {expr : expr option}
| IfThenElseStm of {cond : expr; thbr : statement; elbro : statement option}
| WhileStm of {cond : expr; body : statement}
| ForStm of { init : for_init option 
            ; cond : expr option
            ; update : expr option
            ; body : statement }
| BreakStm
| ContinueStm
| CompoundStm of {stms : statement list}
| ReturnStm of {ret : expr option}

type param = Param of {paramname : ident; typ : typ}

type funtype = FunTyp of {ret : typ; params : param list}

type field = Field of 
         { name  : fieldname 
         ; typ   : typ
         }

type func = Func of {ret : typ; name : ident; args : (ident * typ) list; fbody : statement list}
type rcrd = Rcrd of {name : ident; fields : field list}


type funcOrRcrd =
| Rcr of rcrd
| Fun of func

type program = funcOrRcrd list