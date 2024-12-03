module TAst = TypedAst
module Sym = Symbol
module Run = RuntimeBindings
module Loc = Location

exception Unimplemented (* your code should eventually compile without this exception *)
exception NotPossible (* For when things are not possible *)

(* AUXILIARY FUNCTIONS BELOW *)
(*###########################*)

let find_op_type (op: TAst.binop) : TAst.typ = 
  let open TAst in 
  match op with
  | (Mul|Div|Minus|Plus) -> Int
  | (Lor|Land|Le|Ge|Lt|Gt|Rem|Eq|NEq) -> Bool

(* Convert Ast.Ident to string *)
let ident_to_string (Ast.Ident {name = nm; loc}) = (nm, loc)


(* Convert name of type string to a symbol*)
let convert_str_to_sym name =
  Sym.symbol name

let string_of_sym (sym: Sym.symbol) : string = 
  Sym.name sym 

(* Do a lookup in the environment and get a varOrFun Option *)
let get_varOrFun env sym =
  Env.lookup_var_fun env sym


let type_mismatch_gen (env: Env.environment) (got: TAst.typ) (exp: TAst.typ) (loc : Loc.location) = 
  if got <> exp then
    Env.insert_error env (TypeMismatch {actual = got; expected = exp; loc})
  else 
    ()  

and err_expected_var_gen (env: Env.environment) (expected: string) (loc : Loc.location) =
  Env.insert_error env (Errors.ExpectedVar {expected = expected; loc})


and err_expected_fun_gen (env: Env.environment) (expected: string) (loc : Loc.location) = 
  Env.insert_error env (Errors.ExpectedFun {expected = expected; loc})
and err_arg_length_gen (env: Env.environment) (fname: string) (got: int) (expected: int) (loc : Loc.location) = 
  Env.insert_error env (Errors.ArgLengthNotEqual {fname=fname; actual=got; expected=expected; loc})

and err_empty_program_gen env (loc : Loc.location) = 
  Env.insert_error env @@ Errors.EmptyProgram { loc }

and err_binop_type_mismatch env got expected (loc : Loc.location) =
  if got <> expected then
    Env.insert_error env (BinOpTypeMisMatch {actual = got; expected = expected; loc})
  else 
    ()
and err_expr_stmt env (loc : Loc.location)=
  Env.insert_error env @@ Errors.InvalidExpressionStatement {loc}

and err_void_assignment env (loc : Loc.location) =
    Env.insert_error env @@ VoidAssignment {loc}

and err_missing_return_statemeent_gen (env: Env.environment) (loc : Loc.location) =
  Env.insert_error env @@ MissingReturnStatement {loc; fname = env.fname}

and err_not_in_loop (env : Env.environment) (loc : Loc.location) =
  Env.insert_error env @@ NotInLoop {loc}
  
let zip_param_and_args (params : TAst.param list) (args: (TAst.expr * TAst.typ) list) : (TAst.param * (TAst.expr * TAst.typ)) list = 
  List.combine params args

let ast_ident_to_tast_ident (ident: Ast.ident) : TAst.ident = 
  let (sm, _) = ident_to_string ident in 
  TAst.Ident {sym = (Sym.symbol sm)}

let rec check_stmts_terminate (env : Env.environment) (stmt : Ast.statement) : Env.environment =
  match stmt with 
  | CompoundStm {stms; _} -> 
    begin
    match stms with 
    | [] -> Env.set_all_branch_ret env false
    | _ -> List.fold_left check_stmts_terminate env stms
    end
  | ReturnStm {ret = Some _; _} -> 
    Env.set_all_branch_ret env true
  | IfThenElseStm {thbr; elbro = Some elbro; _} ->
    let thenenv = check_stmts_terminate env thbr in
    let elseenv = check_stmts_terminate env elbro in 
    if thenenv.does_all_branch_ret && elseenv.does_all_branch_ret then
      Env.set_all_branch_ret env true
    else 
      Env.set_all_branch_ret env false
  | _ -> env

let get_args_exp args =
  List.map (fun (s, _) -> s) args

let get_param_type (param : TAst.param) : TAst.typ =
  match param with
  | TAst.Param ({typ; _}) -> typ

let get_arg_position (e: Ast.expr) : Loc.location =
  let open Ast in 
  match e with
  | Integer {loc; _} -> loc
  | Boolean {loc; _} -> loc
  | BinOp {loc; _} -> loc
  | Lval (Var (Ident{loc; _})) -> loc
  | Assignment {loc; _} -> loc
  | Call {loc; _} -> loc
  | UnOp {loc; _} -> loc
  | CommaExpr {loc; _ } -> loc
  | RcrdCreate {loc; _} -> loc
  | ArrayCreate {loc; _} -> loc
  | Lval Idx {loc; _} -> loc
  | Lval Fld {loc; _} -> loc
  | StrLit {loc; _} -> loc
  | LengthOf {loc; _} -> loc




(* Convert Ast.Ident to TAst.Ident *)
let ast_ident_to_tast_ident ast_ident =
  (* Convert Ast.Ident to a string *)
  let (str_name, _) = ident_to_string ast_ident in
  (* Convert string to a symbol*)
  let sym_name = convert_str_to_sym str_name in
  (TAst.Ident {sym = sym_name})

let rec ast_typ_to_tast_typ (tp : Ast.typ) : TAst.typ =
  match tp with 
  | Ast.Int _ -> TAst.Int
  | Ast.Bool _ -> TAst.Bool
  | Ast.Void _ -> TAst.Void
  | Ast.Record {recordname = RecordName {name; _}; _} -> TAst.Record {rname = RecordName {name}}
  | Ast.Array {typ; _} -> TAst.Array {tp = ast_typ_to_tast_typ typ}
  | Ast.Str _ -> TAst.String

(* Convert Ast.Lval to an Ast.Ident *)
let ast_lval_to_ast_ident ast_lval =
  match ast_lval with
  | Ast.Var x -> x

(* Convert TAst.typ to TAst.Ident*)
let tast_typ_to_tast_lval_tp typ sym_var =
  (TAst.Lval (Var {ident = Ident {sym = sym_var}; tp = typ}), typ) 


(* "Calculate_unop" for lack of imagination. Find  if minus sign before a boolean (return err) or "!" sign before number (return err) else return the type *)
let calculate_unop (env: Env.environment) (unop_op: Ast.unop) (e: TAst.expr * TAst.typ) =
    match unop_op with
    (* The op is "-" *)
    | Ast.Neg {loc} -> if snd e <> TAst.Int 
      then
        (* If the op was "-" but the expression was not an integer, we give an err *) 
        let _ = type_mismatch_gen env (snd e) TAst.Int loc in 
        (TAst.UnOp {op=Neg; operand = fst e; tp=ErrorType}, TAst.ErrorType) 
      else 
        (* If the op was "-" and the expression was an integer, we return TAst.expr * TAst.typ *)
        (TAst.UnOp {op=Neg; operand=fst e; tp = TAst.Int}, TAst.Int)

    (* The op is "!" (A prefix that can be used for booleans) *)
    | Ast.Lnot {loc} -> if snd e <> Bool 
      then 
        (* If the op was "!" but the expression was not a boolean, we give an err *)
        let _ = type_mismatch_gen env (snd e) TAst.Bool loc in 
        (TAst.UnOp {op=Lnot; operand = fst e; tp=TAst.ErrorType}, TAst.ErrorType) 
      else 
        (* If the op was "!" and the expression was a boolean, we return TAst.expr * TAst.typ *)
        (TAst.UnOp {op=Lnot; operand = fst e; tp=TAst.Bool}, TAst.Bool)

(* It has already been checked that left_typ == right_typ, so*)
(* Now we need to check that the type of op (+|-|*|/) is the same as left_typ *)  
let binop_check_typing (env: Env.environment) (op_type: TAst.binop) (res: TAst.expr) (l_res: TAst.expr * TAst.typ) (r_res: TAst.expr * TAst.typ) (loc : Loc.location)=
      let left_expr = fst l_res in
      let left_typ = snd l_res in
      let right_expr = fst r_res in
      match op_type with
      | (Plus|Minus|Mul|Div|Rem) -> 
        if left_typ <> Int then 
          let _ = err_binop_type_mismatch env left_typ Int loc in 
          (res, TAst.ErrorType) 
        else 
          (TAst.BinOp {left = left_expr; op = op_type; right = right_expr; tp = TAst.Int}, TAst.Int)
      | (Eq| NEq ) -> 
        if left_typ = Void then 
          let _ = err_binop_type_mismatch env left_typ Void loc in
          (res, TAst.ErrorType) 
      else 
        (TAst.BinOp {left = left_expr; op = op_type; right = right_expr; tp = left_typ}, TAst.Bool)
      | (Lor|Land) -> 
        if left_typ <> Bool then 
          let _ = err_binop_type_mismatch env left_typ Int loc in 
          (res, TAst.ErrorType)  
        else
          (TAst.BinOp {left = left_expr; op = op_type; right = right_expr; tp = TAst.Bool}, TAst.Bool)
      | (Lt|Le|Gt|Ge) ->
        if left_typ <> Int then 
          let _ = err_binop_type_mismatch env left_typ Int loc in 
          (res, TAst.ErrorType)  
        else
          (TAst.BinOp {left = left_expr; op = op_type; right = right_expr; tp = TAst.Bool}, TAst.Bool)

(* 
  This function is only called when we know the expr is lval
  This means other types are not possible.
*)
let find_lval_in_expr expr =
  match expr with
  | TAst.Lval lvl -> lvl
  | _ -> raise NotPossible 

let rec typecheck_typ = function
| Ast.Int _ -> TAst.Int
| Ast.Bool _ -> TAst.Bool
| Ast.Void _ -> TAst.Void
| Ast.Record {recordname = RecordName {name; _}; _} -> 
  TAst.Record {rname = RecordName {name}}
| Ast.Array {typ; _} -> 
  TAst.Array {tp = typecheck_typ typ}
| Ast.Str _ -> TAst.String


let tast_of_op (op : Ast.binop) : TAst.binop =
  match op with
  | Plus _ -> TAst.Plus
  | Minus _ -> TAst.Minus
  | Mul _ -> TAst.Mul
  | Div _ -> TAst.Div
  | Rem _ -> TAst.Rem
  | Lt _ -> TAst.Lt 
  | Le _ -> TAst.Le
  | Gt _ -> TAst.Gt
  | Ge _ -> TAst.Ge
  | Lor _ -> TAst.Lor
  | Land _ -> TAst.Land
  | Eq _ -> TAst.Eq
  | NEq _ -> TAst.NEq
(* Takes a list of expressions and infers them and returns them as a pair of tast expr and typ*)
let rec infer_args (env: Env.environment) (args: Ast.expr list) : (TAst.expr * TAst.typ ) list =
  let helper (acc: (TAst.expr * TAst.typ) list) (elem: Ast.expr) : (TAst.expr * TAst.typ ) list = 
    let inf_exp : (TAst.expr * TAst.typ) = infertype_expr env elem in
    inf_exp :: acc in
  List.fold_left helper [] args

and lookup_func (env: Env.environment) (fname : Ast.ident) : Env.varOrFun option = 
  let (id_name, _) = ident_to_string fname in
  Env.lookup_var_fun env (Sym.symbol id_name)
and check_params_and_args env params args ret fname (loc : Loc.location) : (TAst.expr * TAst.typ) =
  let arg_exps = get_args_exp args in 
  let fname_typed = ast_ident_to_tast_ident fname in  
  let helper_checker (acc : bool) (e : (TAst.param * (TAst.expr * TAst.typ))) =
    let param = get_param_type (fst e) in 
    (* let arg_position = get_arg_position (fst (snd e)) in *)
    let _ = type_mismatch_gen env (snd (snd e)) param loc in (* Type checks each param with arg only reports if doesnt match *)
    if acc then 
    (* return param typ = right pair typ *)
    let expr_typ = snd (snd e) in
    param = expr_typ
    else false in 
  if List.length params <> List.length args 
  then 
    let (fidstring, loc) = ident_to_string fname in
    let _ = err_arg_length_gen env fidstring (List.length args) (List.length params) loc in 
    (TAst.Call {fname = fname_typed; args = arg_exps; tp = ErrorType}, ErrorType)
  else 
    let list_zip = zip_param_and_args params args in 
    let check = List.fold_left helper_checker true list_zip in 
    if check then 
    (* Every thing is good *) 
    (TAst.Call {fname = fname_typed; args = arg_exps; tp = ret}, ret)
    else 
      (TAst.Call {fname = fname_typed; args = arg_exps; tp = ErrorType}, ErrorType)

and lookup_fun (args : (TAst.expr * TAst.typ) list) (fname : Ast.ident) (env : Env.environment) (loc : Loc.location): (TAst.expr * TAst.typ) =
  let open TAst in 
  (* args list, fname ident, env Env.environment*)
  let lookup = lookup_func env fname in 
  match lookup with
  | Some (typ) ->
    begin
    match typ with
    | Fun (FunTyp {ret; params}) -> 
      (* Type check all arguments and return a bool if all agree *)
      check_params_and_args env params args ret fname loc
    | _ -> 
      let (fstring, loc) = ident_to_string fname in 
      let _ = err_expected_fun_gen env fstring loc in
      let arg_exps = get_args_exp args in 
      let fname_typed = ast_ident_to_tast_ident fname in  
      (TAst.Call {fname = fname_typed; args = arg_exps; tp = ErrorType}, ErrorType)
    end 
  | None -> 
    let (fstring, loc) = ident_to_string fname in 
    let _ = err_expected_fun_gen env fstring loc in
    let arg_exps = get_args_exp args in 
    let fname_typed = ast_ident_to_tast_ident fname in  
    (TAst.Call {fname = fname_typed; args = arg_exps; tp = ErrorType}, ErrorType)    


and get_trcrd_fields (Rcrd {fields; _}: TAst.rcrd) : TAst.field list = 
    fields 

and cmp_fields (Field {name = FieldName {name}; _}: TAst.field) (Field {name = FieldName {name = nam2}; _} : TAst.field) : bool =
  name = nam2

and get_trcrd_name (Rcrd {name = Ident {sym}; _}: TAst.rcrd) : Sym.symbol = 
  sym 


and typecheck_fields (env : Env.environment) (rcrd : TAst.rcrd) (fields : (Ast.fieldname * Ast.expr) list) : (TAst.fieldname * TAst.expr) list = 
  (* Check all fields are present in the record *)  
  let record_fields = get_trcrd_fields rcrd in 
  let helper (acc : (TAst.fieldname * TAst.expr) list) (efields : Ast.fieldname * Ast.expr) : (TAst.fieldname * TAst.expr) list =
    let (Ast.FieldName {name = elem_name; _}) = fst efields in 
    let Ast.FieldName {loc; _} = fst efields in 
    let find_hlp (Field {name = FieldName {name; _}; _} : TAst.field) =
      name = elem_name in 
    let tmp_list = List.filter find_hlp record_fields in 
    if List.length tmp_list > 1 then (
      (* Throw error that there are duplicate field assignments in the expr*)
      Env.insert_error env @@ Errors.FieldDoesNotExist {fname = elem_name; loc = loc};
      acc
      )
    else if List.length tmp_list < 1 then ( 
      (* Throw error that the field doesn`t exist in the record *)
      Env.insert_error env @@ Errors.FieldDoesNotExist {fname = elem_name; loc = loc};
      acc
      )
    else
      (* Accept this *)
      let TAst.Field {typ; _} = List.hd tmp_list in
      let texp = typecheck_expr env (snd efields) typ loc in 
      let res = TAst.FieldName{name = elem_name}, texp in 
      res :: acc in 
  List.fold_left helper [] fields

(* should return a pair of a typed expression and its inferred type.
   You can/should use typecheck_expr inside infertype_expr. *)
and infertype_expr (env: Env.environment) (expr: Ast.expr) : (TAst.expr * TAst.typ) =
  let open TAst in 
  match expr with
  | Integer {int = number; _} -> 
    (Integer {int = number}, Int) 
  | Boolean {bool = tf; _} ->
    (Boolean {bool = tf}, Bool)
  | BinOp {left = e1; op = op; right = e2; loc} -> 
    (* First check left side, then right *)
    (* If they have same expression return that otherwise error *)
    let l_res = infertype_expr env e1 in 
    let r_res = infertype_expr env e2 in 
    let op_typed = tast_of_op op in 
    let err_res = BinOp {left = fst l_res; op = op_typed; right = fst r_res; tp= ErrorType} in 
    (* Check that the type of left and right are the same, if not: return err*)
    if (snd l_res <> snd r_res) then
      let _ = err_binop_type_mismatch env (snd l_res) (snd r_res) loc in 
      (err_res, ErrorType)
    else 
      (* Check the type of left and op to see if they are the same *)
      let res = BinOp {left = fst l_res; op = op_typed; right = fst r_res; tp = (find_op_type op_typed)} in 
      binop_check_typing env op_typed res l_res r_res loc

  | UnOp {op = unop_op; operand = unop_operand; _ } ->
    let e = infertype_expr env unop_operand in
    calculate_unop env unop_op e
  | Lval lval -> infertype_lval env lval
  | Call {fname = ident; args = arg_list; loc} ->
    let inf_arg = infer_args env arg_list in 
    lookup_fun inf_arg ident env loc
  | Assignment {lvl; rhs; loc} ->
    let lval_inf = infertype_lval env lvl in
    let lval_res = find_lval_in_expr (fst lval_inf) in
    let expr_inf = infertype_expr env rhs in
    let lval_type = snd lval_inf in
    let expr_type = snd expr_inf in
    if expr_type <> lval_type (* <> is != in ocaml*)
      then (
        (* Make Assignment of {lvl : lval; rhs : expr; tp : typ} *)
        let _ = type_mismatch_gen env (expr_type) (lval_type) loc in 
        (TAst.Assignment {lvl = lval_res; rhs = fst expr_inf; tp = ErrorType}, ErrorType))
      else 
        if expr_type = Void
        then (
          let _ = err_void_assignment env loc in
          (TAst.Assignment {lvl = lval_res; rhs = fst expr_inf; tp = ErrorType}, ErrorType))
        else (
          (TAst.Assignment {lvl = lval_res; rhs = fst expr_inf; tp = expr_type}, expr_type))
  | CommaExpr {left; right; _} -> 
    (* Infer left and right, set type to right expr*)
    let inf_left = infertype_expr env left in
    let inf_right = infertype_expr env right in
    (TAst.CommaExpr {left = fst inf_left; right = fst inf_right; tp = snd inf_right}, snd inf_right)
  | RcrdCreate {recordname = RecordName {name; _}; fields; loc} -> 
    (* 
       1. Check record exists.
       2. Check all the field requirements.
    *)
    let p_rcrd = get_varOrFun env @@ Sym.symbol name in 
    begin
    match p_rcrd with 
    | Some (Rcd (rcrd)) -> 
      (* Check the fields *)
      let tfields = typecheck_fields env rcrd fields in 
      let name = Sym.name @@ get_trcrd_name rcrd in 
      (TAst.RcrdCreate {recordname = RecordName {name}; fields = tfields}, TAst.Record {rname = TAst.RecordName {name}})
    | _ -> 
      (* Throw error, the record does not exist *)
      Env.insert_error env @@ Errors.ExpectedRcrd {rname = name; loc};
      (TAst.RcrdCreate {recordname = RecordName {name}; fields = []}, TAst.ErrorType)
    end
  | ArrayCreate {size; loc; typ} -> 
    (* Check that the declared type exists. (Might be a non existing rcrd.)*)
    let ttyp = ast_typ_to_tast_typ typ in
    let inf_exp = typecheck_expr env size TAst.Int loc in 
    begin 
      match ttyp with 
    | Record {rname = RecordName {name}} -> 
      let pt_rcrd = get_varOrFun env @@ Sym.symbol name in
      begin
        match pt_rcrd with 
        | Some _ -> 
          (* This means the record exists and therefor it typechecked *)
          (TAst.ArrayCreate {size = inf_exp; tp = Array {tp = ttyp}}, Array {tp = ttyp})
        | None -> 
          Env.insert_error env @@ Errors.RecordMisMatch {loc; expected = TAst.Record {rname = RecordName {name}}};
          (TAst.ArrayCreate {size = inf_exp; tp = ErrorType}, ErrorType)
      end
    | _ -> 
      (TAst.ArrayCreate {size = inf_exp; tp = Array {tp = ttyp}}, Array {tp = ttyp})
    end
    | StrLit {str; _} -> 
      (TAst.StrLit {str}, TAst.String)
    | LengthOf {str; loc} -> 
      let inf_exp = typecheck_expr env str TAst.String loc in
      (TAst.LengthOf {str = inf_exp}, TAst.Int)

and does_field_exist (tfields : TAst.field list) (FieldName {name = cmpname; _}: Ast.fieldname) : bool =
  let helper (acc : bool) (Field {name = FieldName {name}; _} : TAst.field) : bool =
    if 
      acc 
    then 
      acc 
    else 
      cmpname = name in 
  List.fold_left helper false tfields

and get_fieldname (FieldName {name; _}: Ast.fieldname) = 
  name

and get_field_type (fields : TAst.field list) (fname : string) : TAst.typ = 
  let helper (Field {name = FieldName {name}; _}: TAst.field) : bool = 
    name = fname in 
  let tmp_list = List.filter helper fields in 
  let mpper (Field {typ; _} : TAst.field) : TAst.typ = 
    typ in 
  let tmp2_list = List.map mpper tmp_list in 
  List.hd tmp2_list


and ast_lval_to_tast_expr_tp env lval =
  match lval with 
  | Ast.Var ast_ident  -> 
  (* Conver Ident to a string *)
  let (name_str, loc) = ident_to_string ast_ident in
  (* Convert name "x" into a symbol *)
  let lvl_sym = convert_str_to_sym name_str in
  (* Get var or function from our symbol *)
  let var_or_fun = get_varOrFun env lvl_sym in
    begin
    match var_or_fun with 
    | Some (Env.Var var_tp) -> tast_typ_to_tast_lval_tp var_tp lvl_sym
    | _ -> 
      (*Specifc error for expected val not fun?*)
      let _ = err_expected_var_gen env name_str loc in 
      (TAst.Lval (Var {ident = Ident {sym = lvl_sym}; tp = ErrorType}), ErrorType)
    end
  | Ast.Fld {rcrd; field; loc} -> 
    let inf_rcrd, rtp = infertype_expr env rcrd in 
    (* Check inf_rcrd is a record *)
    begin
    match rtp with 
    | Record {rname = RecordName {name}} -> 
      (* Lookup the rcrd in env *)
      let pot_rcrd = get_varOrFun env @@ Sym.symbol name in 
      let fname = get_fieldname field in 
      begin
        match pot_rcrd with 
        | Some (Env.Rcd (Rcrd {fields; _})) -> 
          (* Check the field exist *)
          let fexist = does_field_exist fields field in 
          if fexist then 
            (
            (* Get the record from the env *)
            let tp = get_field_type fields fname in 
            let tfld = TAst.Fld {rcrd = inf_rcrd; field = TAst.FieldName {name = fname}; tp; rcrdname = name} in
            (TAst.Lval tfld, tp)
            )
          else 
            ( 
            (* Insert error since the field does not exist. *)
            Env.insert_error env @@ Errors.FieldDoesNotExist {fname; loc = loc};
            (* inf_rcrd might be the wrong expr but doesnt matter since it gives an error *)
            (inf_rcrd, TAst.ErrorType)
            )
        | _ -> 
          (* Throw an error that the record did not exist. *)
          Env.insert_error env @@ Errors.RecordMisMatch {loc; expected = TAst.Record {rname = RecordName {name}}};
          (inf_rcrd, TAst.ErrorType)
      end
    | _ -> 
      (*Throw error rcrd epression was not record *)
      Env.insert_error env @@ Errors.ExprNotARecord {loc};
      (inf_rcrd, TAst.ErrorType)
    end
  | Ast.Idx {arr; index; loc} -> 
    let inf_arr, atp = infertype_expr env arr in 
    let inf_idx, itp = infertype_expr env index in 
    if itp <> Int then 
      let _ = type_mismatch_gen env itp Int loc in 
      (TAst.Lval (Idx {arr = inf_arr; index = inf_idx; tp = ErrorType}), ErrorType)
    else 
      begin
      match atp with 
      | Array {tp} -> 
        (TAst.Lval (Idx {arr = inf_arr; index = inf_idx; tp}), tp)
      | _ -> 
        (* Throw error that the expression is not an array *)
        Env.insert_error env @@ Errors.ExprNotAnArray {loc};
        (TAst.Lval (Idx {arr = inf_arr; index = inf_idx; tp = ErrorType}), ErrorType)
      end


and infertype_lval env lvl =
  ast_lval_to_tast_expr_tp env lvl

(* checks that an expression has the required type tp by inferring the type and comparing it to tp. *)
and typecheck_expr (env : Env.environment) (expr : Ast.expr) (tp: TAst.typ) (loc : Loc.location) =
  let texpr, texprtp = infertype_expr env expr in
  match tp with 
  | Record {rname = RecordName {name = tname}} -> 
    begin 
      match texprtp with 
      | Record {rname = RecordName {name = ename};} -> 
        if ename <> tname then Env.insert_error env (TypeMismatch {actual = texprtp; expected = tp; loc});
        texpr
      | _ -> 
        if texprtp != tp then Env.insert_error env (TypeMismatch {actual = texprtp; expected = tp; loc});
        texpr
    end
  | Array {tp = ttyp} -> 
    begin
    match texprtp with 
    | Array {tp = etyp} -> 
      if etyp <> ttyp then Env.insert_error env (TypeMismatch {actual = texprtp; expected = tp; loc});
      texpr
    | _ -> 
      if texprtp != tp then Env.insert_error env (TypeMismatch {actual = texprtp; expected = tp; loc});
      texpr
    end
  | _ -> if texprtp != tp then Env.insert_error env (TypeMismatch {actual = texprtp; expected = tp; loc});
  texpr


(* Check that the last stmt in the program (stmt list) is a return statement. This is a requirement for Phase 1 *)
let check_for_return_statement (statements: Ast.statement list) =
  let rev_prog = List.rev statements in
  match rev_prog with
  | h::_ -> 
    begin
    match h with
    | Ast.ReturnStm {ret = _; _} -> true
    | _ -> 
      false
    end
  | [] ->
    false

let check_return_stmts (env : Env.environment) (stms : Ast.statement list) : unit =
  let is_last_ret = check_for_return_statement stms in
  if not is_last_ret then 
    let branch_env = List.fold_left check_stmts_terminate env stms in 
    if not branch_env.does_all_branch_ret then 
      Env.insert_error branch_env @@ Errors.MissingReturnStatement {loc = branch_env.func_loc; fname = branch_env.fname}
  

let rec check_elbro env elbro =  (*Has to be above typecheck_statement because of AND mutual usage *)
  match elbro with
  | Some stmt -> 
    Some (fst (typecheck_statement env stmt))
  | _ -> None 
(* should check the validity of a statement and produce the corresponding typed statement. Should use typecheck_expr and/or infertype_expr as necessary. *)

and typecheck_var_stmt sname body tp env (loc : Loc.location) : (TAst.single_declaration * Env.environment) = 
    let tname = TAst.Ident {sym = Symbol.symbol sname} in 
    let (tbody, tbodytp) =
      match tp with
      | None -> infertype_expr env body
      | Some btp -> (typecheck_expr env body (typecheck_typ btp) loc, typecheck_typ btp)
    in
    if tbodytp = TAst.Void 
      then 
        let _ = err_void_assignment env loc in 
       (TAst.Declaration {name = tname; tp = tbodytp; body = tbody},
        Env.insert_local_decl env (Symbol.symbol sname) ErrorType)
    else
    (TAst.Declaration {name = tname; tp = tbodytp; body = tbody},
     Env.insert_local_decl env (Symbol.symbol sname) tbodytp)

and typecheck_statement (env: Env.environment) (stm: Ast.statement) : (TAst.statement * Env.environment) =
  let helper_decl_block (acc: TAst.single_declaration list * Env.environment) ((Ast.Declaration {name; tp; body; loc}) : Ast.single_declaration) =
    let (fstring, _) = ident_to_string name in (* We will be using the decleration location instead of the var name*)
    let (texp, new_env) = typecheck_var_stmt fstring body tp (snd acc) loc in 
    (texp :: (fst acc), new_env)
    in 
  match stm with
  | Ast.VarDeclStm (Ast.DeclBlock {declarations; _}) ->
    (* QUESTION TA HELP TODO regarding loc here. *)
    let res : (TAst.single_declaration list * Env.environment)= List.fold_left helper_decl_block  ([], env) declarations in
    let tmp : TAst.declaration_block = TAst.DeclBlock (fst res) in 
    (TAst.VarDeclStm tmp,
    (snd res))
  | Ast.ExprStm {expr = e1; loc} -> 
    begin
    match e1 with
    | Some exp -> 
      let (texp, _) = infertype_expr env exp in
      let tst : TAst.expr option = Some (texp) in 
      begin
      match texp with 
      | TAst.Call _ | TAst.Assignment _ -> 
        let expstmt = TAst.ExprStm {expr = tst} in  
        (expstmt, env)
      | _ -> 
        let expstmt = TAst.ExprStm {expr = None} in  
        Env.insert_error env @@ Errors.InvalidExpressionStatement {loc};
        (expstmt, env)
      end
    | _ -> 
      let expstmt = TAst.ExprStm {expr = None} in  
      (expstmt, env)
    end
  | Ast.IfThenElseStm {cond; thbr; elbro; loc} -> 
    let inf_cond = infertype_expr env cond in 
    let inf_then = typecheck_statement env thbr in 
    let inf_elbro = check_elbro env elbro in 
    if snd inf_cond <> Bool
    then 
      let _ = type_mismatch_gen env (snd inf_cond) Bool loc in 
      (TAst.IfThenElseStm {cond = fst inf_cond; thbr = fst inf_then; elbro = inf_elbro}, env)
    else 
      (TAst.IfThenElseStm {cond = fst inf_cond; thbr = fst inf_then; elbro = inf_elbro}, env)
  | Ast.ReturnStm {ret = None ; loc} -> 
    if env.ret_type = TAst.Void then 
      (TAst.ReturnStm {ret = None}, env)
    else 
      (* Raise error cant have expression when returning void *)
      let _ = Env.insert_error env @@ Errors.MissingRetExpr {loc; fname = env.fname} in
      (TAst.ReturnStm {ret = None}, env)
  | Ast.ReturnStm {ret = Some ret_ast; loc} -> 
    let tmp = infertype_expr env ret_ast in
    let tmp_type = snd tmp in
    let ret_type = env.ret_type in
    (* TODO CHECK BELOW WORKS *)
    if tmp_type <> TAst.Void 
      then (
        if tmp_type <> ret_type then (
          Env.insert_error env @@ Errors.TypeMismatch {expected = ret_type; actual = tmp_type; loc};
          (TAst.ReturnStm {ret = Some (fst tmp)}, env)
        ) else (
          (TAst.ReturnStm {ret = Some (fst tmp)}, env)
        )
      ) else (
          let _ = Env.insert_error env @@ Errors.RetExprInVoid {loc} in
          (TAst.ReturnStm {ret = Some (fst tmp)}, env)
      )
  | Ast.CompoundStm {stms; _} -> 
    (* Remember to return original enviroment, and use acc env in the compound*)
    let helper acc elem =
      let (li, new_env) = typecheck_statement (snd acc) elem in 
      let new_list : TAst.statement list = li :: (fst acc) in 
      (new_list, new_env) in
    let res = List.fold_left helper ([], env) stms in 
    (TAst.CompoundStm {stms = (fst res)}, env)
  | Ast.WhileStm {cond; body; loc} -> 
    (* First eval cond, check that cond is boolean, if not return error, then eval body*)
    let cond_check = typecheck_expr env cond TAst.Bool loc in 
    let env_loop = Env.set_in_loop env true in 
    let body_eval = typecheck_statement env_loop body in 
    (TAst.WhileStm {cond = cond_check; body = (fst body_eval)}, (snd body_eval))
  | Ast.ForStm {init; cond; update; body; loc} -> 
    begin
    match init with 
    | Some init -> 
      begin
      match init with
      | FIExpr e -> 
        let check_init = infertype_expr env e in 
        let env_loop = Env.set_in_loop env true in 
        let check_body = typecheck_statement env_loop body in 
        begin 
        match cond with 
        | Some cond -> 
          let check_cond = typecheck_expr env cond TAst.Bool loc in 
          begin
          match update with
          | Some update -> 
            let check_update = infertype_expr env update in  
            let tast_res = TAst.ForStm {init = Some (TAst.FIExpr (fst check_init)); cond = Some check_cond; update = Some (fst check_update); body = (fst check_body)} in
            (tast_res, env)
          | None -> (* Should do nothing special when there is no update statement *)
            let tast_res = TAst.ForStm {init = Some (TAst.FIExpr (fst check_init)); cond = Some check_cond; update = None; body = (fst check_body)} in
            (tast_res, env)
          end
        | None -> 
            let tast_res = TAst.ForStm {init = Some (TAst.FIExpr (fst check_init)); cond = Some (TAst.Boolean {bool = true}); update = None; body = (fst check_body)} in
            (tast_res, env)
        end
      | FIDecl (Ast.DeclBlock {declarations; loc}) -> 
        let (tdecl_list, new_env) : (TAst.single_declaration list * Env.environment)= List.fold_left helper_decl_block  ([], env) declarations in
        let env_loop = { new_env with inloop = true } in 
        let check_body = typecheck_statement env_loop body in 
        begin 
        match cond with 
        | Some cond -> 
          let check_cond = typecheck_expr new_env cond TAst.Bool loc in 
          begin
          match update with
          | Some update -> 
            let check_update = infertype_expr new_env update in  
            let tast_res = TAst.ForStm {init = Some (TAst.FIDecl (TAst.DeclBlock tdecl_list)); cond = Some check_cond; update = Some (fst check_update); body = (fst check_body)} in
            (tast_res, env)
          | None -> (* Should do nothing special when there is no update statement *)
            let tast_res = TAst.ForStm {init = Some (TAst.FIDecl (TAst.DeclBlock tdecl_list)); cond = Some check_cond; update = None; body = (fst check_body)} in
            (tast_res, env)
          end
        | None -> 
            let tast_res = TAst.ForStm {init = Some (TAst.FIDecl (TAst.DeclBlock tdecl_list)); cond = Some (TAst.Boolean {bool = true}); update = None; body = (fst check_body)} in
            (tast_res, env)
        end
      end 
    | None -> 
        (* When no initialization do nothing *)
        let env_loop = Env.set_in_loop env true in 
        let check_body = typecheck_statement env_loop body in 
        begin 
        match cond with 
        | Some cond -> 
          let check_cond = typecheck_expr env cond TAst.Bool loc in 
          begin
          match update with
          | Some update -> 
            let check_update = infertype_expr env update in  
            let tast_res = TAst.ForStm {init = None; cond = Some check_cond; update = Some (fst check_update); body = (fst check_body)} in
            (tast_res, env)
          | None -> (* Should do nothing special when there is no update statement *)
            let tast_res = TAst.ForStm {init = None; cond = Some check_cond; update = None; body = (fst check_body)} in
            (tast_res, env)
          end
        | None -> 
            let tast_res = TAst.ForStm {init = None; cond = Some (TAst.Boolean {bool = true}); update = None; body = (fst check_body)} in
            (tast_res, env)
        end
    end
  | BreakStm {loc}  -> 
    (* 1. Check we are in a loop
       2. If not raise error, else add TAst of break/continue*)
    let in_loop = Env.get_in_loop env in 
    if in_loop <> true 
    then 
      let _ = err_not_in_loop env loc in
      (TAst.BreakStm, env)
    else 
      (TAst.BreakStm, env)
  | ContinueStm {loc} ->
    let in_loop = Env.get_in_loop env in
    if in_loop <> true
      then
        let _ = err_not_in_loop env loc in
        (TAst.ContinueStm, env)
    else
      (TAst.ContinueStm, env)
  
  (* should use typecheck_statement to check the block of statements. *)
and typecheck_statement_seq (env: Env.environment) (stms: Ast.statement list) =
let (tstms, _) =
  List.fold_left
    (fun (tsmts_so_far, env_so_far) stm ->
      let (tstm, new_env) = typecheck_statement env_so_far stm in (tstm :: tsmts_so_far, new_env) ) ([], env) stms
in
let res = List.rev tstms in
if env.ret_type = TAst.Void then 
  res 
else 
  let _ =  check_return_stmts env stms in
  res

let convert_args_to_params (args : (Ast.ident * Ast.typ) list) : TAst.param list = 
  (* Fold the args and accumulate them into a param list *)
  let helper (acc : TAst.param list) (elem : (Ast.ident * Ast.typ)) : TAst.param list = 
    let param = TAst.Param {paramname = ast_ident_to_tast_ident @@ fst elem; 
      typ = ast_typ_to_tast_typ @@ snd elem} in 
    param :: acc in 
  let res = List.fold_left helper [] args in 
  res
  

let add_function_names_to_env (env : Env.environment) (funcs : Ast.func list) : (Env.environment) = 
  let helper (acc : (Sym.symbol * TAst.funtype) list) ((Func {name; ret; args; _}) : Ast.func) : (Sym.symbol * TAst.funtype) list = 
    (* Add the func f to the list in the acc *)
    let sname = ident_to_string name in 
    let fname = Sym.symbol @@ fst sname in  
    (* Convert the args to a TAst param list *)
    let params = convert_args_to_params args in 
    (* make a pair of ret and params and add it to the acc *)
    let pair = TAst.FunTyp {ret = ast_typ_to_tast_typ ret; params = params} in 
    (fname, pair) :: acc in
  (* Return the result from a fold *)
  let res = List.fold_left helper [] funcs in 
  let flist = res @ RuntimeBindings.library_functions in 
  Env.extend_func_env env flist
  
let ast_args_to_tast_args (args : (Ast.ident * Ast.typ) list) : (TAst.ident * TAst.typ) list =
  let helper acc (elem : (Ast.ident * Ast.typ)) =
    let tident = ast_ident_to_tast_ident @@ fst elem in 
    let ttyp = ast_typ_to_tast_typ @@ snd elem in 
    (tident, ttyp) :: acc in 
  List.fold_left helper [] args 

let add_args_to_env (env :Env.environment) (args : (Ast.ident * Ast.typ) list) : Env.environment =
  let helper (acc : Env.environment) (elem: (Ast.ident * Ast.typ )): Env.environment =
    let sname = Sym.symbol @@ fst @@ ident_to_string @@ fst elem in 
    let ttyp = ast_typ_to_tast_typ @@ snd elem in 
    Env.insert_local_decl acc  sname ttyp in
  List.fold_left helper env args 

let get_ident_loc (Ident {loc; _}: Ast.ident) : Loc.location = 
  loc

let check_dublicate_args (env : Env.environment) (args : (Ast.ident * Ast.typ) list) : unit =
  (* This works. For a shorter algorithem that does roughly the same, look at duplicate functions *)
  (* That method is clearner and doesn`t make unessecary lists. I hope. *)
  let get_first_in_pair_list (acc : Ast.ident list) (elem : Ast.ident * Ast.typ) = 
    fst elem :: acc in 
  let idList = List.fold_left get_first_in_pair_list [] args in 
  let helper (_ : unit) (elem : Ast.ident) : unit = 
    let id, loc = ident_to_string elem in
    let check_eq arg = 
      let id_arg, _ = ident_to_string arg in 
      id_arg = id in
    let all_elems = List.filter check_eq idList in
    if List.length all_elems > 1 then 
      (* insert error of dublicate parameters *)
      Env.insert_error env (Errors.DublicateParameter {id; loc}) in
  List.fold_left helper () idList

let get_funs_in_prog (prog : Ast.program) : Ast.func list = 
  let helper (elem : Ast.funcOrRcrd) : Ast.func option = 
    match elem with
    | Fun f -> Some f 
    | _ -> None in 
  List.filter_map helper prog

let does_main_exist (prog : Ast.program) : bool =
  let helper (acc : bool) (Func {name = Ident {name; _}; _}: Ast.func) =
    if acc then acc 
    else
      if name = "main" then true else acc in
  let f_list = get_funs_in_prog prog in 
  List.fold_left helper false f_list

let get_func_name (Func {name = Ident {name; _}; _}: Ast.func) : string =
  name

let rec get_rcrds_in_prog (prog : Ast.program) : Ast.rcrd list = 
  let helper (elem : Ast.funcOrRcrd) : Ast.rcrd option = 
    match elem with 
    | Rcr (r) -> Some r
    | Fun _ -> None in
  List.filter_map helper prog 

let convert_fields_to_tfields (fields : Ast.field list) : TAst.field list = 
  let helper (Field {name = FieldName {name; _}; typ; _} : Ast.field) : TAst.field =
    let ttyp = ast_typ_to_tast_typ typ in 
    TAst.Field {name = TAst.FieldName {name = name}; typ = ttyp} in 
  List.map helper fields

let convert_ast_rcrds_to_tast (rcrds : Ast.rcrd list) : TAst.rcrd list =
  let helper (Recor {recordname = RecordName {name; _}; fields} : Ast.rcrd) : TAst.rcrd = 
    let tfields = convert_fields_to_tfields fields in 
    TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol name}; fields = tfields} in 
  List.map helper rcrds

let convert_tast_rcrds_to_tastprog (trcrds : TAst.rcrd list) : TAst.program =
  let helper (r : TAst.rcrd) : TAst.funcOrRcrd =
    TAst.Rcr r in 
  List.map helper trcrds

(* Should only be called when it is known a main function exists otherwise an exception is thrown. *)
let get_main_func (prog : Ast.program) : Ast.func =
  (* Below is another way to find first *)
  let helper (elem : Ast.funcOrRcrd) : bool = 
    match elem with
    | Rcr _ -> false
    | Fun f ->
    if get_func_name f = "main" then 
      (* Only called when we know a main func exists *)
      true
    else 
      false in 
  (* We know only one exists as function names are unique *)
  let li = List.filter helper prog in
  (* This means we can use hd from list to get the first and only elem *)
  let fs = get_funs_in_prog li in 
  List.hd fs 

let check_no_main_params (env : Env.environment) (funcs : Ast.program) : unit =
  let Func {args; loc; _}= get_main_func funcs in 
  if List.length args <> 0 then 
    Env.insert_error env @@ Errors.MainHasParameters {loc}

let check_main_has_type_int (env : Env.environment) (funcs : Ast.program) : unit =
  let Func {ret; loc; _} = get_main_func funcs in
  if ast_typ_to_tast_typ ret <> TAst.Int then 
    Env.insert_error env @@ Errors.MainHasWrongRetType {loc}

let check_main_demands (env : Env.environment) (funcs : Ast.program) : unit =
  let does_main_exist = does_main_exist funcs in 
  if does_main_exist then (
    (* Check there are no args *)
    check_no_main_params env funcs;
    check_main_has_type_int env funcs)
  else 
    Env.insert_error env @@ Errors.MissingMainMethod
  
let check_all_function_names_are_unique (env : Env.environment) (funcs : Ast.func list) : unit =
  let check_fname_is_unique (_ : unit) (Func {name = Ident {name; _}; loc; _}: Ast.func) : unit = 
    let filter_pred (arg : Ast.func) = 
      if get_func_name arg = name then true else false in 
    let elem_name_list = List.filter filter_pred funcs in 
    if List.length elem_name_list > 1 then 
      (* Insert error *)
      Env.insert_error env @@ Errors.NotUniqueMethodName {loc} in
  List.fold_left check_fname_is_unique () funcs

let check_rcrd_names_are_unique (env: Env.environment) (rcrds : Ast.rcrd list) : unit = 
  (*
    1. Check that name only occurs once in rcrds (meaning its unique)
    2. Check that the name is not colliding with names from std library.
  *)
  let chck_rcrds (_ : unit) (Recor {recordname = RecordName {name; loc; _};_} : Ast.rcrd) : unit = 
    let check_r (Recor {recordname = RecordName {name = oname; _}; _} : Ast.rcrd) : bool =
      if oname = name then 
        true 
      else 
        false in 
    let filtered = List.filter check_r rcrds in 
    if List.length filtered <> 1 
      then 
      (* There are more than one declarations of the name *)
      let _ = Env.insert_error env @@ Errors.DuplicateRecordDecleration {rname = name; loc = loc} in
      ()
      else 
      () in 
  List.fold_left chck_rcrds () rcrds;
  let chck_std_lib (_ : unit) (Recor {recordname = RecordName {name; loc; _}; _} : Ast.rcrd) : unit =
    if List.mem name Run.rsv_rcrds
      then 
        (* Insert error *)
      let _ = Env.insert_error env @@ Errors.StdLibraryRecprd {rname = name; loc} in
      ()
      else
      () in 
  List.fold_left chck_std_lib () rcrds

let rec convert_rcrds_to_string_list (rcrds : Ast.rcrd list) : string list =
  let helper (acc : string list) (Recor {recordname = RecordName {name; _}; _} : Ast.rcrd) : string list = 
    name :: acc in
  List.fold_left helper [] rcrds
 
  (* Checks that if a field is a record, that the field exists *)
let rec typecheck_field (fieldname : string) (env : Env.environment) (tp : Ast.typ) loc rcrds : unit =
  match tp with 
  | Array {typ; _} -> typecheck_field fieldname env typ loc rcrds
  | Record {recordname = RecordName {name; _}; _} -> 
    let srcrds = convert_rcrds_to_string_list rcrds in 
    begin
    if List.mem name srcrds then 
      ()
    else 
      let _ = Env.insert_error env @@ Errors.ReferringToNonExistingRcrd {loc; fieldname; rname = name} in 
      () 
    end
  | _ -> () (* It is a basic type then it must be okay *)

let convert_fields_to_string_list (fields : Ast.field list) : string list =
  let field_conv (Ast.Field {name = FieldName {name; _}; _} : Ast.field) = 
    name in
  List.map field_conv fields 


let check_field_is_unique (env : Env.environment) (fieldname : string) loc (fields : Ast.field list) : unit =
  let sfields = convert_fields_to_string_list fields in 
  let eqf f = if fieldname = f then true else false in 
  let fieldnameList = List.filter eqf sfields in 
  if List.length fieldnameList <> 1 
    then 
    (* raise error *)
    let _ = Env.insert_error env @@ Errors.DuplicateField {fieldname; loc} in
    ()
    else 
      ()

let check_field (env : Env.environment) (Recor {recordname = RecordName {name = rname; _}; fields; _}: Ast.rcrd) rcrds : TAst.rcrd =
  let helper (acc : TAst.field list) (Ast.Field {name = FieldName {name; loc}; typ; _}) : TAst.field list = 
    (* Check field name is unique and typecheck it. After convert it to TAst.field *)
    let _ = typecheck_field name env typ loc rcrds in 
    let _ = check_field_is_unique env name loc fields in
    TAst.Field {name = TAst.FieldName {name}; typ = ast_typ_to_tast_typ typ} :: acc in 
  let tfields = List.fold_left helper [] fields in 
  TAst.Rcrd {name = TAst.Ident {sym = Sym.symbol rname}; fields = tfields}

(* checks the field and returns the typed rcrd with the typed fields *)
let check_fields (env : Env.environment) (rcrds : Ast.rcrd list) : TAst.rcrd list = 
  (* 
     1. Check field names are unique 
     2. Check that each fieldname has a valid type
  *)
  let helper (acc : TAst.rcrd list) (elem : Ast.rcrd) : TAst.rcrd list = 
    let r = check_field env elem rcrds in 
    r :: acc in
  List.fold_left helper [] rcrds

let get_trcrd_name (TAst.Rcrd {name = Ident {sym}; _} : TAst.rcrd) : Sym.symbol = 
  sym

let convert_trcrds_to_envr (trcrds : TAst.rcrd list) : (Sym.symbol * TAst.rcrd) list =
  let helper (acc : (Sym.symbol * TAst.rcrd) list) (elem : TAst.rcrd) : (Sym.symbol * TAst.rcrd) list =
    let s = get_trcrd_name elem in 
    (s, elem) :: acc in 
  List.fold_left helper [] trcrds

let check_rcrds_are_valid (env: Env.environment) (rcrds : Ast.rcrd list) : Env.environment =
  (* 
    1. check rcrd name is unique
    2. check fields are mapping to a correct type. 
  *)
  check_rcrd_names_are_unique env rcrds;
  let trcrds = check_fields env rcrds in
  let strcrds = convert_trcrds_to_envr trcrds in 
  Env.extend_rcrd_env env strcrds 

let rec check_rcrd_tp (env : Env.environment) (tp : Ast.typ) : TAst.typ = 
  match tp with
  | Array {typ; _} -> check_rcrd_tp env typ 
  | Record {recordname = RecordName {name; loc}; _} ->
    let rcrd_or_err = get_varOrFun env @@ Sym.symbol name in
    begin
    match rcrd_or_err with 
    | Some (Env.Rcd trcd) -> TAst.Record {rname = TAst.RecordName {name = string_of_sym @@ get_trcrd_name trcd}}
    | _ -> 
      Env.insert_error env @@ Errors.RecordMisMatch {expected = ast_typ_to_tast_typ tp; loc};
      TAst.ErrorType
    end
  | _ -> ast_typ_to_tast_typ tp 

let typecheck_function_seq (prog : Ast.program) : (TAst.program * Env.environment) = 
  (* First check the records *)
  (*
    1. Add records to a list
    2. Check all records in the list    
  *)
  let rcrds = get_rcrds_in_prog prog in
  (* Make an empty environment where all keyworded rcrds are in*)
  let init_env = Env.init_rcrd_env in 
  let rcrd_env = check_rcrds_are_valid init_env rcrds in 

  (* First add all function names to runtime bindings *)

  (* Now that all functions have been added, run semantics on each body *)
  let f_env = add_function_names_to_env rcrd_env @@ get_funs_in_prog prog in

  (* Second for each function, add its argument to the env, then typecheck_statement_seq on the body *)
  let helper (acc : TAst.program) ((Func {name; ret; fbody; args; loc}) : Ast.func) : TAst.program = 
    (* Evaluating a function goes like this : *)

    (* Setup env information of the current function in the env*)
    (* Add args to env *)
    let new_env = add_args_to_env f_env args in 
    let new_ret_env = Env.set_ret_type new_env @@ ast_typ_to_tast_typ ret in 

    (* Add the function name and location *)
    let tmp_env = Env.set_fname new_ret_env @@ fst @@ ident_to_string name in
    let fin_env = Env.set_func_loc tmp_env loc in 

    (* Semant the body *)
    let tbody = typecheck_statement_seq fin_env fbody in 
    (* The retstms will be checked to match ret in typecheck seq *)

    (* Check that there are no dublicate names in the args *)
    let _ = check_dublicate_args f_env args in

    (* Convert the Ast.func to TAst.func *)
    let fname = ast_ident_to_tast_ident name in 
    (* Convet Ast.args to TAst.args *)
    let targs = ast_args_to_tast_args args in 

    (* Check that the function type is valid *)
    let tret = check_rcrd_tp f_env ret in 

    let func = TAst.Fun (TAst.Func{
      ret = tret;
      fbody = tbody;
      name = fname;
      args = targs;
    }) in

    func :: acc in 

    (* Check that there the only function named main both exists and takes no arguments *)
    let _ = check_main_demands f_env prog in 
    let _ = check_all_function_names_are_unique f_env @@ get_funs_in_prog prog in 

  (* The call to the helper function *)
  let flist = List.fold_left helper [] @@ get_funs_in_prog prog, f_env in
  let rprog = convert_tast_rcrds_to_tastprog @@ convert_ast_rcrds_to_tast rcrds in
  fst flist @ rprog, snd flist

(* the initial environment should include all the library functions, no local variables, and no errors. *)
let initial_environment () = 
  Env.make_env RuntimeBindings.library_functions
