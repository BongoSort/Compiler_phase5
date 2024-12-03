module Sym = Symbol
module B = CfgBuilder
module TAst = TypedAst
let symbol = Sym.symbol 
exception NotImplemented
exception NotPossible
 
type cg_env =      
   { cfgb: CfgBuilder.cfg_builder ref
   ; locals: (Ll.ty * Ll.operand) Sym.Table.t
   ; rcds: (Ll.uid * Ll.ty list) Sym.Table.t
   ; rcrd_map: Ll.uid Sym.Table.t
   ; rcrdlist : TAst.rcrd list 
   ; str_map : (Ll.gid * Ll.gdecl) Sym.Table.t
   ; loops: (Sym.symbol * Sym.symbol)
   ; rettyp: (Ll.ty)
   }      

let rec tast_type_to_lltype (env : cg_env) typ = 
  match typ with
  | TAst.Void -> Ll.Void
  | TAst.Int -> Ll.I64
  | TAst.Bool -> Ll.I1
  | TAst.Record {rname = TAst.RecordName {name}} -> 
    let rcrds = env.rcrd_map in 
    let rcrd = Sym.Table.find (Sym.symbol name) rcrds in
    Ll.Ptr (Ll.Namedt rcrd)
  | TAst.ErrorType -> 
    print_endline "this is it";
    raise NotPossible (* Errortype can never exist *)
  | TAst.Array {tp} -> 
    Ll.Ptr (tast_type_to_lltype env tp)
  | TAst.String -> Ll.Ptr (Ll.Namedt (Sym.symbol "string_type"))

let rec get_default_value typ = 
  match typ with
  | TAst.Int -> Ll.IConst64 0L
  | TAst.Bool -> Ll.BConst false
  | TAst.Record _ -> 
    Ll.IConst64 0L
  | TAst.ErrorType ->
    raise NotPossible (* Errortype can never exist *)
  | TAst.Void -> Ll.IConst64 0L
  | TAst.Array _ -> 
    Ll.IConst64 0L
  | TAst.String -> 
    Ll.IConst64 0L

let rec tast_arr_typ_create (env : cg_env) typ = 
  match typ with 
  | TAst.Array {tp} -> 
    tast_type_to_lltype env tp
  | TAst.Int -> Ll.I64
  | TAst.Bool -> Ll.I1
  | TAst.Record {rname = TAst.RecordName {name}} -> 
    let rcrds = env.rcrd_map in 
    let rcrd = Sym.Table.find (Sym.symbol name) rcrds in
    Ll.Ptr (Ll.Namedt rcrd)
  | TAst.ErrorType ->
    print_endline "this is it";
    raise NotPossible (* Errortype can never exist *)
  | TAst.Void -> Ll.Void
  | TAst.String -> Ll.Void 

let compare (Field {name = FieldName {name = x}; _}: TAst.field) (Field {name = FieldName {name = y}; _} : TAst.field) =
  if x < y then -1
  else if x > y then 1
  else 0;;

let cmp_rcrd_fields ((TAst.FieldName {name = x}, _):(TAst.fieldname * TAst.expr)) ((TAst.FieldName {name = y}, _):(TAst.fieldname * TAst.expr))= 
  if x < y then -1
  else if x > y then 1
  else 0;;

  
let expr_to_lltype env expr =
  match expr with
  | TAst.Integer {int = _} -> Ll.I64
  | TAst.Boolean {bool = _} -> Ll.I1
  | TAst.BinOp {tp; _} -> tast_type_to_lltype env tp
  | TAst.UnOp {tp; _} -> tast_type_to_lltype env tp
  | TAst.Lval TAst.Var {tp;_} -> tast_type_to_lltype env tp
  | TAst.Assignment {tp; _} -> tast_type_to_lltype env tp
  | TAst.Call {tp; _} -> tast_type_to_lltype env tp
  | TAst.CommaExpr {tp; _} -> tast_type_to_lltype env tp
  | TAst.ArrayCreate {tp; _} -> tast_type_to_lltype env tp
  | TAst.Lval TAst.Fld {tp; _} -> tast_type_to_lltype env tp
  | TAst.RcrdCreate {recordname = RecordName {name}; _} -> 
    let rcrds = env.rcrd_map in
    let rcrd = Sym.Table.find (Sym.symbol name) rcrds in
    Ll.Ptr (Ll.Namedt rcrd)
  | TAst.Lval TAst.Idx {tp; _} -> tast_type_to_lltype env tp
  | TAst.StrLit {str = _} -> Ll.Ptr (Ll.I8)
  | TAst.LengthOf {str = _} -> Ll.I64

let fresh_symbol =
  let c = ref 0 in
  fun initial ->
    let n = !c in c := n + 1; Sym.symbol (initial ^ (string_of_int n))

let emit env b = 
  let current_builder = !(env.cfgb) in 
  let new_builder = b current_builder in 
  env.cfgb := new_builder

let get_field_index (env : cg_env) (rcrdname : string) (fname : string) : int =
  let rcrds = env.rcrdlist in
  let helper (Rcrd {name = Ident {sym = name}; _} : TAst.rcrd) : bool =
    Sym.name name = rcrdname in
  let (Rcrd {fields; _}) = List.find helper rcrds in 
  let sfields = List.sort compare fields in 
  let helper_index (index, bo : int * bool) (Field {name = FieldName {name}; _} : TAst.field) : int * bool = 
    if bo || name = fname then index, true
      else (index + 1, false) in 
  let res, _ = List.fold_left helper_index (0, false) sfields in 
  res

let get_rcrd_typ (env : cg_env) (rcrdname : string) : Ll.ty = 
  let table = env.rcds in 
  let res = Sym.Table.find (Sym.symbol rcrdname) table in 
  Ll.Namedt (fst res)

let calc_elem_size (env : cg_env) (tp : TAst.typ) =
    let array_gep = fresh_symbol "array_gep_" in
    let ltp = tast_type_to_lltype env tp in
    let elem_size = fresh_symbol "elem_size_" in
    match tp with 
    | TAst.Record _ | TAst.Void -> 
      emit env @@ CfgBuilder.add_insn (Some array_gep, Ll.Gep (ltp, Ll.Null, [Ll.IConst32 1l])); 
      emit env @@ CfgBuilder.add_insn (Some elem_size, Ll.Ptrtoint (ltp, Ll.Id array_gep, Ll.I32));
      Ll.Id elem_size
    | TAst.Array _ -> 
      (* Should perhabs not be a null reference. *)
      emit env @@ CfgBuilder.add_insn (Some array_gep, Ll.Gep (ltp, Ll.Null, [Ll.IConst32 1l])); 
      emit env @@ CfgBuilder.add_insn (Some elem_size, Ll.Ptrtoint (ltp, Ll.Id array_gep, Ll.I32));
      Ll.Id elem_size
    | TAst.Bool | TAst.Int -> 
      emit env @@ CfgBuilder.add_insn (Some array_gep, Ll.Gep (ltp, Ll.Null, [Ll.IConst32 1l])); 
      emit env @@ CfgBuilder.add_insn (Some elem_size, Ll.Ptrtoint (ltp, Ll.Id array_gep, Ll.I32));
      Ll.Id elem_size
    | TAst.String -> 
      emit env @@ CfgBuilder.add_insn (Some array_gep, Ll.Gep (ltp, Ll.Null, [Ll.IConst32 1l])); 
      emit env @@ CfgBuilder.add_insn (Some elem_size, Ll.Ptrtoint (ltp, Ll.Id array_gep, Ll.I32));
      Ll.Id elem_size
    | TAst.ErrorType -> 
      raise NotPossible (* Errortype can never exist *)

let rec trans_expr (env:cg_env) expr =   
  let emit = emit env in 
  let emit_insn_with_fresh hint inst  =  
        let tmp = fresh_symbol hint in 
        emit @@ CfgBuilder.add_insn (Some tmp, inst); 
        Ll.Id tmp  in
  let setup_fields (env : cg_env) (rcrdname : Sym.symbol) (fields : (TAst.fieldname * TAst.expr) list) (rcrdtyp : Ll.ty) (allocptr : Sym.symbol) = 
    let sfields = List.sort cmp_rcrd_fields fields in 
    let helper (acc : int) (( FieldName {name}, exp) : TAst.fieldname * TAst.expr) : int = 
      let ptr_field = fresh_symbol @@ "ptr_field_" ^ name in
      let expr_op = trans_expr env @@ exp in 
      emit @@ CfgBuilder.add_insn (Some ptr_field, 
      Ll.Gep (
        rcrdtyp, Ll.Id allocptr, [Ll.IConst64 0L; Ll.IConst32 (Int32.of_int acc)]
      ));


      let rcrd_field_tp_list = snd @@ Sym.Table.find rcrdname @@ env.rcds in
      let fieldtyp = List.nth rcrd_field_tp_list acc in

      emit @@ CfgBuilder.add_insn (None, 
      Ll.Store (
        fieldtyp, expr_op, Ll.Id ptr_field
      )
      );
      acc + 1 in
    List.fold_left helper 0 sfields in 
  let _tr = trans_expr env in 
  let binop_helper_bool left right ret_tp = 
    let l_op = _tr left in 
    let r_op = _tr right in  
    emit_insn_with_fresh "cmp" 
      @@ Ll.Icmp (ret_tp, Ll.I64, l_op, r_op) in
  let binop_helper_int left right op =
    let l_op = _tr left in 
    let r_op = _tr right in  
    emit_insn_with_fresh "tmp" 
      @@ Ll.Binop (op, Ll.I64, l_op, r_op) in  
  match expr with 
  | TAst.Integer {int} -> Ll.IConst64 int 
  | TAst.Boolean {bool} -> Ll.BConst bool
  (* BinOps below *)
  | TAst.BinOp {left; op = TAst.Div; right; _} ->
    binop_helper_int left right Ll.SDiv
    (* Move Left to rax*)
    (* Add ctqo instruction *)
    (* Make div instruction *)
  | TAst.BinOp {left; op = TAst.Plus; right; _} -> 
    binop_helper_int left right Ll.Add
  | TAst.BinOp {left; op = TAst.Minus; right; _} ->
    binop_helper_int left right Ll.Sub
  | TAst.BinOp {left; op = TAst.Mul; right; _} ->
    binop_helper_int left right Ll.Mul
  | TAst.BinOp {left; op = TAst.Rem; right; _} -> 
    binop_helper_int left right Ll.SRem
  | TAst.BinOp {left; op = TAst.Ge; right; _} -> 
    binop_helper_bool left right Sge
  | TAst.BinOp {left; op = TAst.Lt; right; _} ->
    binop_helper_bool left right Ll.Slt
  | TAst.BinOp {left; op = TAst.Le; right; _} ->
    binop_helper_bool left right Ll.Sle
  | TAst.BinOp {left; op = TAst.Gt; right; _} ->
    binop_helper_bool left right Ll.Sgt
  | TAst.BinOp {left; op = TAst.Eq; right; _} ->
    binop_helper_bool left right Ll.Eq
  | TAst.BinOp {left; op = TAst.NEq; right; _} ->
    binop_helper_bool left right Ll.Ne
  | TAst.BinOp {left; op = TAst.Lor; right; _} ->
    let after_eval = fresh_symbol "shortor_" in
    let tmp = fresh_symbol "tmp_" in 
    let res = fresh_symbol "res_" in 
    let eval = fresh_symbol "eval_other_" in
    let left_op = trans_expr env left in 
    emit @@ CfgBuilder.term_block (Cbr (left_op, tmp, eval));
    emit @@ CfgBuilder.start_block eval;
    let right_op = trans_expr env right in
    emit @@ CfgBuilder.term_block (Br after_eval);
    emit @@ CfgBuilder.start_block tmp;
    emit @@ CfgBuilder.term_block (Br after_eval);
    emit @@ CfgBuilder.start_block after_eval;
    emit @@ CfgBuilder.add_insn (Some (res), Ll.PhiNode (Ll.I1, [(left_op, tmp); (right_op, eval)]));
    Ll.Id res  
   | TAst.BinOp {left; op = TAst.Land; right; _} ->
    let after_eval = fresh_symbol "shortand_" in
    let tmp = fresh_symbol "tmp" in
    let res = fresh_symbol "res" in 
    let eval = fresh_symbol "eval" in 
    let left_op = trans_expr env left in
    emit @@ CfgBuilder.term_block (Cbr (left_op, eval, tmp));
    emit @@ CfgBuilder.start_block eval;
    let right_op = trans_expr env right in 
    emit @@ CfgBuilder.term_block (Br after_eval);
    emit @@ CfgBuilder.start_block tmp;
    emit @@ CfgBuilder.term_block (Br after_eval);
    emit @@ CfgBuilder.start_block after_eval;
    emit @@ CfgBuilder.add_insn (Some (res), Ll.PhiNode (Ll.I1, [(left_op, tmp); (right_op, eval)]));
    Ll.Id res 
  (* UnOp for Integers *)  
  | TAst.UnOp {op = TAst.Neg; operand; _} ->
    binop_helper_int (TAst.Integer {int = Int64.zero}) operand Ll.Sub
  (* UnOp for Booleans *)
  | TAst.UnOp {op = TAst.Lnot; operand; tp} ->
    let op = _tr operand in 
    let tmp = _tr (TAst.Boolean {bool = true}) in 
    let typ = tast_type_to_lltype env tp in 
    emit_insn_with_fresh "bool" 
      @@ Ll.Binop (Ll.Xor, typ, op, tmp)
  | TAst.Lval (Var {ident = (Ident {sym}); tp = _tp}) -> 
    let (ty, sym_op) = Sym.Table.find sym (env.locals) in 
    let load_ins = Ll.Load (ty, sym_op) in 
    emit_insn_with_fresh (Sym.name sym) load_ins
  | TAst.Assignment {lvl= (TAst.Var { ident = (TAst.Ident {sym}); _}); rhs; _} -> 
    let env_lcl = env.locals in
    let (sym_tp, sym_op) = Sym.Table.find sym env_lcl in
    let op = trans_expr env rhs in
    let llins = Ll.Store (sym_tp, op, sym_op) in
    emit @@ CfgBuilder.add_insn (None, llins);
    op
  | TAst.Call {fname = TAst.Ident {sym}; args : TAst.expr list; tp : TAst.typ} ->
    let r_args = List.rev args in
    let helper acc elem =
      let r = expr_to_lltype env elem in 
      let op = trans_expr env elem in 
      let tmp = (r, op) in 
      tmp :: acc in 
    let ags = List.fold_left helper [] r_args in
    let instruction = Ll.Call (tast_type_to_lltype env tp, (Ll.Gid sym), ags) in
    if tp = TAst.Void
       then
        (let _ = emit @@ CfgBuilder.add_insn (None, instruction) in
        Ll.Null)
      else  
      emit_insn_with_fresh "call_" instruction
  | TAst.CommaExpr {left; right; _} -> 
    let _ = trans_expr env left in
    let right = trans_expr env right in
    right
  | TAst.Lval Fld {rcrd; field = FieldName {name}; rcrdname; _} -> 
    let r_eval = trans_expr env rcrd in 
    let field_rndex = get_field_index env rcrdname name in 
    let rcrd_tp = get_rcrd_typ env rcrdname in
    let fresh_elem = fresh_symbol "elem_ptr_" in 
    emit @@ CfgBuilder.add_insn (Some (fresh_elem), 
    Ll.Gep (rcrd_tp, r_eval, [Ll.IConst64 0L; Ll.IConst32 (Int32.of_int field_rndex)])
    );
    let map = env.rcds in
    (* Convert the pointer to the field type *)
    let rcrd_tp_list = snd @@ Sym.Table.find (Sym.symbol rcrdname) map in
    let fieldtyp = List.nth rcrd_tp_list field_rndex in
    let fresh_elem_val = fresh_symbol "elem_val_" in
    emit @@ CfgBuilder.add_insn (Some (fresh_elem_val),
    Ll.Load (fieldtyp, Ll.Id fresh_elem)
    );
    Ll.Id fresh_elem_val
  | TAst.RcrdCreate {recordname = RecordName {name}; fields} -> 
    (* Codegen for record declaration *)
    (*
      1. Get the struct being created
      2. Call runtime func to allocate space on the heap for the record
      3. Set the fields in the struct to the value given.    
    *)

    let rstruct = Sym.Table.find (Sym.symbol name) env.rcrd_map in 

    (* Getting the size *)
    let size_ptr = fresh_symbol "size_ptr_" in 
    emit @@ CfgBuilder.add_insn (Some size_ptr, Ll.Gep
    (
    Ll.Namedt rstruct, Ll.Null, [IConst32 1l]
    ));
    (* Cast ptr to int *)
    let size = fresh_symbol "size_" in 
    emit @@ CfgBuilder.add_insn (Some size, Ll.Ptrtoint (
    Ll.Namedt rstruct, (Ll.Id size_ptr), Ll.I32));
    (* Call into the runtime for allocation *)
    let ptr_rt = fresh_symbol "ptr_rt" in 
    let alloc_op = Ll.Gid (Sym.symbol "allocate_record") in 
    emit @@ CfgBuilder.add_insn (Some (ptr_rt), Ll.Call
    (
      Ll.Ptr (Ll.I8), alloc_op, [Ll.I32, Ll.Id size]
    ));
    let rcrd_ptr = fresh_symbol @@ name ^ "_" in 
    emit @@ CfgBuilder.add_insn (Some rcrd_ptr, 
    Ll.Bitcast (Ll.Ptr (Ll.I8), Ll.Id ptr_rt, Ll.Ptr (Ll.Namedt rstruct))
    );
    (* Save the fields *)
    let _ = setup_fields env (Sym.symbol name) fields (Ll.Namedt rstruct) rcrd_ptr in 
    Ll.Id rcrd_ptr
  | ArrayCreate {tp; size} -> 
    let size_op = trans_expr env size in
    let alloc_op = Ll.Gid (Sym.symbol "allocate_array") in
    let arr_tp = tast_arr_typ_create  env tp in 
    let array_ptr = fresh_symbol "array_ptr_" in
    let init_value_ptr = fresh_symbol "init_value_ptr_" in
    (* Alloca for init value pointer and store 0 in it*)    
    emit @@ CfgBuilder.add_alloca (init_value_ptr, Ll.I8);
    (* Init value should be 0 for int, false for bool, and null for records, empty string for strings *)
    let init_val = get_default_value tp in 
    emit @@ CfgBuilder.add_insn (None, Ll.Store (Ll.I8, init_val, Ll.Id init_value_ptr));
    (* Get size of the type *)
    let elem_size = calc_elem_size env tp in
    emit @@ CfgBuilder.add_insn (Some array_ptr, Ll.Call (Ll.Ptr (Ll.I8), alloc_op, [Ll.I32, elem_size; Ll.I64, size_op; Ll.Ptr (Ll.I8) , Ll.Id init_value_ptr]));
    (* Cast ptr to array_tp *)
    let array_res = fresh_symbol "array_res_" in
    emit @@ CfgBuilder.add_insn (Some array_res, Ll.Bitcast (Ll.Ptr (Ll.I8), Ll.Id array_ptr, Ll.Ptr (arr_tp)));
    Ll.Id array_res
  | TAst.Lval Idx {arr; index; tp;} ->
    let array_ptr = trans_expr env arr in
    let array_ptr_tp = tast_type_to_lltype env tp in
    let array_tp = tast_arr_typ_create env tp in
    let generic_arr_ptr = fresh_symbol "generic_arr_ptr_" in
    let idx = trans_expr env index in 
    (* Check that idx is not higher than array size *)
    let arr_size = fresh_symbol "arr_size_" in
    let idx_ok = fresh_symbol "idx_ok_" in
    let idx_fail = fresh_symbol "idx_fail_" in
    let cmp = fresh_symbol "cmp_" in
    (* Get the generic array pointer from array_ptr *)
    emit @@ CfgBuilder.add_insn (Some generic_arr_ptr, Ll.Bitcast (Ll.Ptr (array_tp), array_ptr, Ll.Ptr (Ll.I64)));

    (* Get the size of the array *)
    emit @@ CfgBuilder.add_insn (Some arr_size, Ll.Call (Ll.I64, Ll.Gid (Sym.symbol "dolphin_rc_get_array_length"), [Ll.Ptr (Ll.I64), Ll.Id generic_arr_ptr]));
    (* Bound check. Check that the field number is less than size*)
    emit @@ CfgBuilder.add_insn (Some cmp, Ll.Icmp (Ll.Sle, Ll.I64, idx, Ll.Id arr_size));
    emit @@ CfgBuilder.term_block @@ Cbr (Ll.Id cmp, idx_ok, idx_fail);
    emit @@ CfgBuilder.start_block idx_fail;
    (* report_error_array_index_out_of_bounds func void no args *)
    emit @@ CfgBuilder.add_insn (None, Ll.Call (Ll.Void, Ll.Gid (Sym.symbol "report_error_array_index_out_of_bounds"), []));
    emit @@ CfgBuilder.term_block @@ Br idx_ok;
    emit @@ CfgBuilder.start_block idx_ok;
    let elem_ptr = fresh_symbol "elem_ptr" in
    emit @@ CfgBuilder.add_insn (Some elem_ptr, Ll.Gep (array_ptr_tp, array_ptr, [idx]));
    let elem = fresh_symbol "elem_" in
    emit @@ CfgBuilder.add_insn (Some elem, Ll.Load (array_tp, Ll.Id elem_ptr));
    Ll.Id elem
  | TAst.Assignment {lvl = Idx {arr; index; tp}; rhs; _} ->
    (* First check if index is out of bounds. *)
      (* eval the index  *)
    let index_op = trans_expr env index in
    let array_op = trans_expr env arr in
    let array_tp = tast_arr_typ_create env tp in
    let array_ptr = fresh_symbol "array_ptr_" in
    let generic_arr_ptr = fresh_symbol "i64_arr_ptr_" in
    let idx_ok = fresh_symbol "idx_ok_" in
    let idx_fail = fresh_symbol "idx_fail_" in
    let cmp = fresh_symbol "cmp_" in
    (* Get the generic array pointer from array_ptr *)
    emit @@ CfgBuilder.add_insn (None, Ll.Comment "Get the i64 array pointer from array_ptr");
    emit @@ CfgBuilder.add_insn (Some generic_arr_ptr, Ll.Bitcast (Ll.Ptr (array_tp), array_op, Ll.Ptr (Ll.I64)));

    (* Get the size of the array *)
    emit @@ CfgBuilder.add_insn (Some array_ptr, Ll.Call (Ll.I64, Ll.Gid (Sym.symbol "dolphin_rc_get_array_length"), [Ll.Ptr (Ll.I64), Ll.Id generic_arr_ptr]));
    (* Bound check. Check that the field number is less than size*)
    emit @@ CfgBuilder.add_insn (Some cmp, Ll.Icmp (Ll.Sle, Ll.I64, index_op, Ll.Id array_ptr));
    emit @@ CfgBuilder.term_block @@ Cbr (Ll.Id cmp, idx_ok, idx_fail);
    emit @@ CfgBuilder.start_block idx_fail;
    (* report_error_array_index_out_of_bounds func void no args *)
    emit @@ CfgBuilder.add_insn (None, Ll.Call (Ll.Void, Ll.Gid (Sym.symbol "report_error_array_index_out_of_bounds"), []));
    emit @@ CfgBuilder.term_block @@ Br idx_ok;
    emit @@ CfgBuilder.start_block idx_ok;
    let elem_ptr = fresh_symbol "elem_ptr" in
    emit @@ CfgBuilder.add_insn (Some elem_ptr, Ll.Gep (array_tp, array_op, [index_op]));
    (* Load value (rhs) in elem_ptr *)
    let rhs_op = trans_expr env rhs in
    emit @@ CfgBuilder.add_insn (None, Ll.Store (array_tp, rhs_op, Ll.Id elem_ptr));
    rhs_op
  | TAst.Assignment {lvl = Fld {rcrd = rcrd_exp; field = FieldName {name}; rcrdname; _}; rhs; _} ->
    emit @@ CfgBuilder.add_insn (None, Ll.Comment "Field assignment.");
    (* Lookup field *)
    let rcrd = env.rcds in
    let rcrd_tp = get_rcrd_typ env rcrdname in
    let field_rndex = get_field_index env rcrdname name in
    let field_ptr = fresh_symbol "field_ptr_" in
    let rcrd_tp_list = snd @@ Sym.Table.find (Sym.symbol rcrdname) rcrd in
    let fieldtp = List.nth rcrd_tp_list field_rndex in
    (* evaluate the rcrd then gep and get the right field. Then store the value in the field *)
    let rcrd_ptr = trans_expr env rcrd_exp in
    emit @@ CfgBuilder.add_insn (None, Ll.Comment "Get the field pointer");
    emit @@ CfgBuilder.add_insn (Some field_ptr, Ll.Gep (rcrd_tp, rcrd_ptr, [Ll.IConst64 0L; Ll.IConst32 (Int32.of_int field_rndex)]));
    (* Store the rhs in the field pointer *)
    let rhs_op = trans_expr env rhs in 
    emit @@ CfgBuilder.add_insn (None, Ll.Store (fieldtp, rhs_op, Ll.Id field_ptr));
    rhs_op
  | TAst.StrLit {str} -> 
    (* This should be a pointer to the global string value in the program *)
    emit @@ CfgBuilder.add_insn (None, Ll.Comment "String literal");
    let str = Sym.Table.find (Sym.symbol str) env.str_map in
    let str_tp = Ll.Ptr (fst @@ snd str) in 
    let string_type = Ll.Ptr (Ll.Namedt (Sym.symbol "string_type")) in 
    let str_op = Ll.Gid (fst str) in
    let str_cast = fresh_symbol "str_cast_" in
    emit @@ CfgBuilder.add_insn (Some str_cast,
    Ll.Bitcast (str_tp, str_op, string_type)
    );
    Ll.Id str_cast
  | TAst.LengthOf {str = e} ->
    raise NotImplemented
    

let is_last_term_ret (env: cg_env) : bool =
  CfgBuilder.is_last_term !(env.cfgb)

let var_stmt env body sym tp =      (* 1. codegen the expression *)
      let emit = emit env in 
      let body_op = trans_expr env body in
      (* 2. codegen allocation for the new variable *)
      (* we need a symbol and a type *)
      let ll_sym = fresh_symbol (Sym.name sym ^ "_loc_") in 
      let llty = tast_type_to_lltype env tp in 
      let b = CfgBuilder.add_alloca (ll_sym, llty) in 
      emit b ;
      (* 3. extend our locals maps *)
      let current_locals = env.locals in 
      let new_locals = (* updated map  *)
          Sym.Table.add sym (llty, Ll.Id ll_sym) current_locals in 
      let new_env = {env with locals = new_locals} in    
      (* 4. store the result of (1) in (2) *)
      emit @@ CfgBuilder.add_insn (None, 
                    Ll.Store (llty, body_op, Ll.Id ll_sym ) );
      new_env
(* takes an environment and a statement 
    - returns an updated environment (with possibly new declarations added into it) *)
let rec trans_stmt (env:cg_env) stm =  
  let emit = emit env in 
  let helper_var_list (acc: cg_env) (TAst.Declaration {name = (TAst.Ident {sym}); tp; body}) = 
      var_stmt acc body sym tp in 
  match stm with 
  | TAst.VarDeclStm (TAst.DeclBlock single_list) -> 
    List.fold_left helper_var_list env single_list
  | ExprStm {expr = (Some expr)} -> 
    let _ = trans_expr env expr in
    env
  | ExprStm {expr = None} ->
    (* Nothing to do really -.- *)
    env

  | TAst.ReturnStm {ret = Some ret} -> 
      let retdump_lbl = fresh_symbol "retdump_" in 
      let ret_op = trans_expr env ret in  (* translating the expression 
                                           ; getting back the LLVM operand 
                                             that has the result 
                                             -- with potentially updated 
                                             -- CFG 
                                          *)    
      emit @@ CfgBuilder.term_block (Ll.Ret (env.rettyp, Some ret_op)) ;
      emit @@ CfgBuilder.start_block retdump_lbl;
      env      
  | TAst.ReturnStm {ret = None} ->
      let retdump_lbl = fresh_symbol "retdump_" in 
      emit @@ CfgBuilder.term_block (Ll.Ret (env.rettyp, None)) ;
      emit @@ CfgBuilder.start_block retdump_lbl;
      env      

  | TAst.IfThenElseStm {cond; thbr; elbro = None} ->
    let c_op = trans_expr env cond in 
    let nm_then = fresh_symbol "then" in 
    let nm_after = fresh_symbol "after" in 
    emit @@ CfgBuilder.term_block (Cbr (c_op, nm_then, nm_after));
    emit @@ CfgBuilder.start_block nm_then;
    let _ = trans_stmt env thbr in 
    emit @@ CfgBuilder.term_block (Br nm_after);
    emit @@ CfgBuilder.start_block nm_after; 
    env
  | TAst.IfThenElseStm {cond; thbr; elbro = (Some elbro)} ->
    
    let c_op = trans_expr env cond in 
    let nm_then = fresh_symbol "then" in 
    let nm_after = fresh_symbol "after" in 
    let nm_else = fresh_symbol "else" in
    emit @@ CfgBuilder.term_block (Cbr (c_op, nm_then, nm_else));
    (* Then *)
    emit @@ CfgBuilder.start_block nm_then;
    let _ = trans_stmt env thbr in 
    emit @@ CfgBuilder.term_block (Ll.Br nm_after);

    (* Else *)
    emit @@ CfgBuilder.start_block nm_else;
    let _ = trans_stmt env elbro in 
    emit @@ CfgBuilder.term_block (Ll.Br nm_after);
    emit @@ CfgBuilder.start_block nm_after;

    (* After *)
    env
  | TAst.CompoundStm {stms} -> 
    let helper acc_env stmt =
      trans_stmt acc_env stmt in
    let rstmt = List.rev stms in 
    let _ = List.fold_left helper env rstmt in 
    env
  | WhileStm {cond; body} -> 
    let while_cond = fresh_symbol "while_cond_" in 
    let while_body = fresh_symbol "while_body_" in 
    let while_after = fresh_symbol "while_after_" in 
    (* Check that break works *)
    let while_env : cg_env = {env with loops = (while_cond, while_after)} in 
    emit @@ CfgBuilder.term_block (Ll.Br while_cond);
    emit @@ CfgBuilder.start_block while_cond;
    let if_lbl = trans_expr while_env cond in 
    emit @@ CfgBuilder.term_block (Ll.Cbr (if_lbl, while_body, while_after));
    emit @@ CfgBuilder.start_block while_body;
    let _ = trans_stmt while_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br while_cond);
    emit @@ CfgBuilder.start_block while_after;
    env
  | ForStm {init = Some(TAst.FIExpr init_expr); cond = Some(cond); update = Some(update); body} -> 
    let init_lbl = fresh_symbol "for_init_" in 
    let cond_lbl = fresh_symbol "for_cond_" in 
    let update_lbl = fresh_symbol "for_update_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    let for_env : cg_env = {env with loops = (update_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br init_lbl); 
    emit @@ CfgBuilder.start_block init_lbl;
    let _ = trans_expr env init_expr in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block cond_lbl;
    let test_cond = trans_expr for_env cond in 
    emit @@ CfgBuilder.term_block (Ll.Cbr (test_cond, body_lbl, after_lbl));
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in
    emit @@ CfgBuilder.term_block (Ll.Br update_lbl);
    emit @@ CfgBuilder.start_block update_lbl;
    let _ = trans_expr for_env update in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = Some(TAst.FIDecl (TAst.DeclBlock e)); cond = Some(cond); update = Some(update); body} -> 
    let init_lbl = fresh_symbol "for_init_" in 
    let cond_lbl = fresh_symbol "for_cond_" in 
    let update_lbl = fresh_symbol "for_update_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    emit @@ CfgBuilder.term_block (Ll.Br init_lbl); 
    emit @@ CfgBuilder.start_block init_lbl;
    let new_env = List.fold_left helper_var_list env e in 
    let for_env : cg_env = {new_env with loops = (update_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block cond_lbl;
    let test_cond = trans_expr for_env cond in 
    emit @@ CfgBuilder.term_block (Ll.Cbr (test_cond, body_lbl, after_lbl));
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in
    emit @@ CfgBuilder.term_block (Ll.Br update_lbl);
    emit @@ CfgBuilder.start_block update_lbl;
    let _ = trans_expr for_env update in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = None; cond = Some(cond); update = Some(update); body} -> 
    let cond_lbl = fresh_symbol "for_cond_" in 
    let update_lbl = fresh_symbol "for_update_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    let for_env : cg_env = {env with loops = (update_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block cond_lbl;
    let test_cond = trans_expr for_env cond in 
    emit @@ CfgBuilder.term_block (Ll.Cbr (test_cond, body_lbl, after_lbl));
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br update_lbl);
    emit @@ CfgBuilder.start_block update_lbl;
    let _ = trans_expr for_env update in
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = Some(TAst.FIDecl (TAst.DeclBlock var_decl_list)); cond = None; update = Some(update); body} -> 
    let init_lbl = fresh_symbol "for_init_" in 
    let update_lbl = fresh_symbol "for_update_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    emit @@ CfgBuilder.term_block (Ll.Br init_lbl);
    emit @@ CfgBuilder.start_block init_lbl;
    let new_env = List.fold_left helper_var_list env var_decl_list in 
    let for_env : cg_env = {new_env with loops = (update_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br update_lbl);
    emit @@ CfgBuilder.start_block update_lbl;
    let _ = trans_expr for_env update in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = Some(TAst.FIExpr e); cond = None; update = Some(update); body} -> 
    let init_lbl = fresh_symbol "for_init_" in 
    let update_lbl = fresh_symbol "for_update_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    let for_env : cg_env = {env with loops = (update_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br init_lbl);
    emit @@ CfgBuilder.start_block init_lbl;
    let _ = trans_expr env e in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br update_lbl);
    emit @@ CfgBuilder.start_block update_lbl;
    let _ = trans_expr for_env update in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = None; cond = None; update = Some(update); body} -> 
    let update_lbl = fresh_symbol "for_update_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    let for_env : cg_env = {env with loops = (update_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br update_lbl);
    emit @@ CfgBuilder.start_block update_lbl;
    let _ = trans_expr for_env update in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = None; cond = None; update = None; body} -> 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    let for_env : cg_env = {env with loops = (body_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = Some(TAst.FIDecl (TAst.DeclBlock var_decl_list)); cond = Some(cond); update = None; body} -> 
    let init_lbl = fresh_symbol "for_init_" in 
    let cond_lbl = fresh_symbol "for_cond_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    emit @@ CfgBuilder.term_block (Ll.Br init_lbl);
    emit @@ CfgBuilder.start_block init_lbl;
    let new_env = List.fold_left helper_var_list env var_decl_list in 
    let for_env : cg_env = {new_env with loops = (cond_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block cond_lbl;
    let test_cond = trans_expr for_env cond in 
    emit @@ CfgBuilder.term_block (Ll.Cbr (test_cond, body_lbl, after_lbl));
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = Some(TAst.FIExpr exp); cond = Some(cond); update = None; body} -> 
    let init_lbl = fresh_symbol "for_init_" in 
    let cond_lbl = fresh_symbol "for_cond_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    let for_env : cg_env = {env with loops = (cond_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br init_lbl);
    emit @@ CfgBuilder.start_block init_lbl;
    let _ = trans_expr env exp in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block cond_lbl;
    let test_cond = trans_expr for_env cond in 
    emit @@ CfgBuilder.term_block (Ll.Cbr (test_cond, body_lbl, after_lbl));
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = None; cond = Some(cond); update = None; body} -> 
    let cond_lbl = fresh_symbol "for_cond_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    let for_env : cg_env = {env with loops = (cond_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block cond_lbl;
    let test_cond = trans_expr for_env cond in 
    emit @@ CfgBuilder.term_block (Ll.Cbr (test_cond, body_lbl, after_lbl));
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br cond_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = Some(TAst.FIExpr exp); cond = None; update = None; body} -> 
    let init_lbl = fresh_symbol "for_init_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    let for_env : cg_env = {env with loops = (body_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br init_lbl);
    emit @@ CfgBuilder.start_block init_lbl;
    let _ = trans_expr env exp in
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | ForStm {init = Some(TAst.FIDecl (TAst.DeclBlock var_decl_list)); cond = None; update = None; body} -> 
    let init_lbl = fresh_symbol "for_init_" in 
    let body_lbl = fresh_symbol "for_body_" in 
    let after_lbl = fresh_symbol "for_after_" in 
    emit @@ CfgBuilder.term_block (Ll.Br init_lbl);
    emit @@ CfgBuilder.start_block init_lbl;
    let new_env = List.fold_left helper_var_list env var_decl_list in 
    let for_env : cg_env = {new_env with loops = (body_lbl, after_lbl)} in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block body_lbl;
    let _ = trans_stmt for_env body in 
    emit @@ CfgBuilder.term_block (Ll.Br body_lbl);
    emit @@ CfgBuilder.start_block after_lbl;
    env
  | BreakStm -> 
    let break_lbl = fresh_symbol "break_" in 
    emit @@ CfgBuilder.term_block (Ll.Br (snd (env.loops)));
    emit @@ CfgBuilder.start_block break_lbl;
    env
  | ContinueStm -> 
    let continue_lbl = fresh_symbol "continue_" in 
    emit @@ CfgBuilder.term_block (Ll.Br (fst (env.loops)));
    emit @@ CfgBuilder.start_block continue_lbl;
    env

  
let trans_stmt_list (env:cg_env) stmts = 
  List.fold_left trans_stmt env stmts

let convert_tastlist_to_ty (env : cg_env) (types : (TAst.ident * TAst.typ) list) =
  let helper acc (elem : TAst.ident * TAst.typ) =
    let t = snd elem in 
    let llt = tast_type_to_lltype env t in 
    llt :: acc in
  List.fold_left helper [] types

let convert_args_to_uid (args : (TAst.ident * TAst.typ) list) : Ll.uid list =
  let helper acc ( Ident {sym}, _ : TAst.ident * TAst.typ) : Ll.uid list = 
    sym :: acc in 
  List.fold_left helper [] args 

let mk_env (tp : Ll.ty) : cg_env =
  let res = { cfgb = ref CfgBuilder.empty_cfg_builder
    ; locals =  Sym.Table.empty
    ; rcds = Sym.Table.empty
    ; rcrd_map = Sym.Table.empty
    ; str_map = Sym.Table.empty
    ; rcrdlist = [] 
    ; loops = (Sym.symbol "", Sym.symbol "")
    ; rettyp = tp
  } in 
  res

let set_ret_env env (tp : Ll.ty) : cg_env = 
  let res = {
    cfgb = env.cfgb
    ; locals = env.locals
    ; rcds = env.rcds
    ; rcrdlist = env.rcrdlist
    ; str_map = env.str_map
    ; rcrd_map = env.rcrd_map
    ; loops = env.loops
    ; rettyp = tp
  } in 
  res

let set_rcds_env env rcds rcdslist : cg_env = 
  let res = {
    cfgb = env.cfgb
    ; locals = env.locals
    ; rcds = rcds 
    ; rcrdlist = rcdslist
    ; rcrd_map = env.rcrd_map
    ; str_map = env.str_map
    ; loops = env.loops
    ; rettyp = env.rettyp
  } in 
  res

let set_str_map_env env str_map : cg_env = 
  let res = {
    cfgb = env.cfgb
    ; locals = env.locals
    ; rcds = env.rcds
    ; rcrdlist = env.rcrdlist
    ; rcrd_map = env.rcrd_map
    ; str_map = str_map
    ; loops = env.loops
    ; rettyp = env.rettyp
  } in 
  res

let set_rcrdmap_env env rcrd_map: cg_env = 
  let res = {
    cfgb = env.cfgb
    ; locals = env.locals
    ; rcds = env.rcds
    ; rcrdlist = env.rcrdlist
    ; str_map = env.str_map
    ; rcrd_map = rcrd_map
    ; loops = env.loops
    ; rettyp = env.rettyp
  } in 
  res

let add_args_to_env (env : cg_env) (args : (TAst.ident * TAst.typ) list) : cg_env = 
  let helper (nenv : cg_env) ((Ident {sym}, typ) : TAst.ident * TAst.typ) : cg_env =
    let arg_sym = fresh_symbol "arg_" in
    let llt = tast_type_to_lltype env typ in 
    emit nenv @@ CfgBuilder.add_alloca (arg_sym, llt);
    emit nenv @@ CfgBuilder.add_insn (None, 
                    Ll.Store (llt , Ll.Id sym, Ll.Id arg_sym) );
    let arg_locl = tast_type_to_lltype env typ, Ll.Id arg_sym in
    let res = Sym.Table.add sym arg_locl nenv.locals in
    let new_env = {env with locals = res} in
    new_env in 
  List.fold_left helper env args

let rec get_funcs_in_prog (acc : TAst.func list) (tprog : TAst.program) : TAst.func list = 
  match tprog with 
  | h :: t ->
    begin
    match h with
    | Fun f -> f :: (get_funcs_in_prog acc t)
    | _ -> get_funcs_in_prog acc t
    end
  | _ -> acc

let get_rcds_in_prog (tprog : TAst.program) : TAst.rcrd list = 
  let hlp_filt (elem : TAst.funcOrRcrd) : TAst.rcrd option = 
    match elem with 
    | Rcr r -> Some r 
    | _ -> None in 
  List.filter_map hlp_filt tprog


let tast_field_to_ll (fields : TAst.field list) env : Ll.ty list =
  let helper (acc : Ll.ty list) (Field {typ; _} : TAst.field) : Ll.ty list =
    tast_type_to_lltype env typ :: acc in 
  List.fold_left helper [] fields

let rec check_expr_for_str_lit (str_map : (Ll.gid * Ll.gdecl) Sym.Table.t) (expr : TAst.expr) : (Ll.gid * Ll.gdecl) Sym.Table.t =
  match expr with 
  | TAst.StrLit {str} -> 
    begin
    let str_sym = Sym.symbol str in 
    let str_lit_name = fresh_symbol "str_lit_" in
    let str_struct = fresh_symbol "str_struct_" in
    let gstring = Ll.GString str in
    let str_length = String.length str in
    let str_lit = Ll.Array (str_length, Ll.I8) in
    let random_symbol = Sym.symbol "random__" in

    (* Create the array structure and a reference to the literal in it *)
    let array_struct = Ll.Struct [Ll.I64; Ll.Ptr (Ll.Array (str_length, Ll.I8))] in
    let garray = Ll.GStruct [Ll.I64, Ll.GInt str_length; Ll.Ptr (Ll.Array (str_length, Ll.I8)), Ll.GGid str_lit_name] in

    (* Create the global string literal *)

    (* Add garray to the str map with a random name *)
    let new_map = Sym.Table.add random_symbol (str_lit_name, (str_lit, gstring)) str_map in
    (* Add the string literal to the str map *)
    Sym.Table.add str_sym (str_struct, (array_struct, garray)) new_map
    end
  | TAst.Assignment {lvl = Idx {arr; index; _}; rhs; _} ->
    let res1 = check_expr_for_str_lit str_map arr in
    let res2 = check_expr_for_str_lit res1 index in
    check_expr_for_str_lit res2 rhs
  | TAst.BinOp {left; right; _} ->
    let res1 = check_expr_for_str_lit str_map left in
    check_expr_for_str_lit res1 right
  | TAst.UnOp {operand; _} ->
    check_expr_for_str_lit str_map operand
  | Lval (Idx {arr; index; _}) ->
    let res1 = check_expr_for_str_lit str_map arr in
    check_expr_for_str_lit res1 index
  | Lval (Fld {rcrd; _}) ->
    check_expr_for_str_lit str_map rcrd
  | Assignment {lvl; rhs; _} -> 
    begin
    match lvl with 
    | Var _ -> 
      let res = check_expr_for_str_lit str_map rhs in 
      res
    | Idx {arr; index; _} ->
      let res1 = check_expr_for_str_lit str_map arr in
      let res2 = check_expr_for_str_lit res1 index in
      check_expr_for_str_lit res2 rhs
    | Fld {rcrd; _} ->
      let res = check_expr_for_str_lit str_map rcrd in 
      res
    end
  | Call {args; _} ->
    let helper (acc : (Ll.gid * Ll.gdecl) Sym.Table.t) (elem : TAst.expr) : (Ll.gid * Ll.gdecl) Sym.Table.t = 
      check_expr_for_str_lit acc elem in 
    List.fold_left helper str_map args
  | RcrdCreate {fields; _} ->
    (* Get all the expressions in the fields, and check that call check_expr_for_str on that *)
    let helper_map (elem : TAst.fieldname * TAst.expr) : TAst.expr = 
      snd elem in 
    let expr_list = List.map helper_map fields in
    let helper (acc : (Ll.gid * Ll.gdecl) Sym.Table.t) (elem : TAst.expr) : (Ll.gid * Ll.gdecl) Sym.Table.t = 
      check_expr_for_str_lit acc elem in
    List.fold_left helper str_map expr_list
  | LengthOf {str} ->
    check_expr_for_str_lit str_map str
  | _ -> str_map

let rec check_statements_for_str (stmts : TAst.statement list) (str_map : (Ll.gid * Ll.gdecl) Sym.Table.t) : (Ll.gid * Ll.gdecl) Sym.Table.t = 
  let helper (acc : (Ll.gid * Ll.gdecl) Sym.Table.t) (stmt : TAst.statement) : (Ll.gid * Ll.gdecl) Sym.Table.t = 
    match stmt with 
    | ExprStm {expr = Some expr} -> check_expr_for_str_lit acc expr
    | VarDeclStm (DeclBlock single_list) -> 
      let helper2 (acc2 : (Ll.gid * Ll.gdecl) Sym.Table.t) (TAst.Declaration {name = (TAst.Ident {sym}); tp; body}) = 
        check_expr_for_str_lit acc2 body in 
      List.fold_left helper2 acc single_list
    | IfThenElseStm {cond; thbr; elbro = None} ->
      let res = check_expr_for_str_lit acc cond in 
      let res2 = check_statements_for_str [thbr] res in 
      res2
    | IfThenElseStm {cond; thbr; elbro = (Some elbro)} ->
      let res = check_expr_for_str_lit acc cond in 
      let res2 = check_statements_for_str [thbr; elbro] res in 
      res2
    | WhileStm {cond; body} ->
      let res = check_expr_for_str_lit acc cond in 
      let res2 = check_statements_for_str [body] res in 
      res2
    | ForStm {init; cond; update; body} ->
      let res = 
        begin
        match init with 
        | Some(TAst.FIExpr e) -> check_expr_for_str_lit acc e
        | Some(TAst.FIDecl (TAst.DeclBlock single_list)) -> 
          let helper2 (acc2 : (Ll.gid * Ll.gdecl) Sym.Table.t) (TAst.Declaration {body; _}) = 
            check_expr_for_str_lit acc2 body in 
          List.fold_left helper2 acc single_list
        | None -> acc
        end in 
      let res2 = 
        begin
        match cond with 
        | Some e -> check_expr_for_str_lit res e
        | None -> res
        end in 
      let res3 = 
        begin
        match update with 
        | Some e -> check_expr_for_str_lit res2 e
        | None -> res2
        end in 
      let res4 = check_statements_for_str [body] res3 in 
      res4
    | CompoundStm {stms} -> 
      let res = check_statements_for_str stms acc in 
      res
    | ReturnStm {ret = Some ret} -> 
      check_expr_for_str_lit acc ret
    | _ -> acc in

  List.fold_left helper str_map stmts



let rec check_functions_for_str (funcs : TAst.func list) (str_map : (Ll.gid * Ll.gdecl) Sym.Table.t) : (Ll.gid * Ll.gdecl) Sym.Table.t =
  let helper (acc : (Ll.gid * Ll.gdecl) Sym.Table.t) (Func {fbody; _} : TAst.func) : (Ll.gid * Ll.gdecl) Sym.Table.t = 
    let stmts = fbody in 
    let res = check_statements_for_str stmts acc in
    res in 
  List.fold_left helper str_map funcs



let helper_generate_rcrd_names (rcrds : TAst.rcrd list) : Ll.uid Sym.Table.t = 
  let helper (acc : Ll.uid Sym.Table.t) (Rcrd {name = Ident {sym}; _}: TAst.rcrd) : Ll.uid Sym.Table.t = 
    let rname = "dlp_rec_" ^ (Sym.name sym) in 
    Sym.Table.add sym (Sym.symbol rname) acc in
  let tmp = List.fold_left helper Sym.Table.empty rcrds in 
  Sym.Table.add (Sym.symbol "stream") (Sym.symbol "dolphin_record_stream") tmp

let get_ll_code (rcds : TAst.rcrd list) env : (Sym.symbol * (Ll.uid * Ll.ty list)) list =
  let rcrd_names = env.rcrd_map in

  let helper (acc : (Sym.symbol * (Ll.uid * Ll.ty list)) list) (Rcrd {name = Ident {sym}; fields}: TAst.rcrd) : (Sym.symbol * (Ll.uid * Ll.ty list)) list = 
    let rcrd_name = Sym.Table.find sym rcrd_names in 
    let sorted_fields = List.sort compare fields in 
    let llfields = tast_field_to_ll sorted_fields env in 
    let rllfields = List.rev llfields in 
    let pair = rcrd_name, rllfields in
    let res = sym, pair in 
    res :: acc in 
  List.fold_left helper [] rcds

let rcd_binding_to_table (types : (Sym.symbol * (Ll.uid * Ll.ty list)) list) : (Ll.uid * Ll.ty list) Sym.Table.t =
  let helper (acc : (Ll.uid * Ll.ty list) Sym.Table.t) (elem : (Sym.symbol * (Ll.uid * Ll.ty list))) : (Ll.uid * Ll.ty list) Sym.Table.t = 
    Sym.Table.add (fst elem) (snd elem) acc in 
  List.fold_left helper Sym.Table.empty types
  (* let streams = Sym.symbol "dolphin_record_stream", [Ll.Struct []] in
  Sym.Table.add (Sym.symbol "stream") streams temp *)


let rcd_binding_to_tdecl (rcds : (Ll.uid * (Ll.uid * Ll.ty list)) list) =
  let helper (elem : Ll.uid * (Ll.uid * Ll.ty list)) =
    let tmp = snd elem in 
    let fieldtyp = Ll.Struct (snd @@ snd elem) in 
    (fst tmp, fieldtyp) in
  List.map helper rcds 

let trans_prog (tprog: TAst.program) =   
  let open Ll in   
  let rcds = get_rcds_in_prog tprog in 
  (* raise Should fix below, i don`t think it supports recursive types atm *)
  let non_imp_env = mk_env Ll.Void in 
  let rcrd_mp = helper_generate_rcrd_names rcds in 
  let nenv = set_rcrdmap_env non_imp_env rcrd_mp in 
  let rcd_bindings = get_ll_code rcds nenv in
  let rcd_table = rcd_binding_to_table rcd_bindings in 

  (* Take all strings in the code and add them to gdecls *)
  let str_map = Sym.Table.empty in
  (* Get all functions *)
  let func_list = get_funcs_in_prog [] tprog in 
  (* Check all expression within the function statements to make sure every stringliteral is added to the table *)
  let nstr_map = check_functions_for_str func_list str_map in
  let nenv = set_str_map_env nenv nstr_map in 

  let helper (acc : (Ll.uid * Ll.fdecl) list) (Func {ret; name = Ident {sym}; args; fbody} : TAst.func) =
    let tmp_env = set_ret_env nenv Ll.Void in 
    let tmp1_env = set_rcds_env tmp_env rcd_table rcds in 
    let rtyp = tast_type_to_lltype tmp1_env ret in  
    let rcd_env = set_ret_env tmp1_env rtyp in 
    (* Add struct references to env *)
    let env_with_args = add_args_to_env rcd_env args in
    let env = trans_stmt_list env_with_args fbody in (* do all the work *)
    
    (* Make the stuff unreachable at the end, if it is not a void function *)
    (* In case it is void, add ret void at the very end. This is because the code could be meaningfull *)
    if ret <> TAst.Void 
      then
        emit env @@ CfgBuilder.term_block Ll.Unreachable
      else 
        emit env @@ CfgBuilder.term_block @@ Ll.Ret (Ll.Void, None);
    let cfg = CfgBuilder.get_cfg !(env.cfgb) in 
    let argtypes = convert_tastlist_to_ty env args in 
    let paruid = convert_args_to_uid args in 
    let fbody = sym, { fty = (argtypes, rtyp); 
    param = paruid; cfg = cfg} in 
    fbody :: acc in

  let prog = get_funcs_in_prog [] tprog in

  (*
  define i64 @dolphin_rc_get_array_length (i64* %array) {
  %len_ptr = getelementptr i64, i64* %array, i64 -1
  %size= load i64, i64* %len_ptr
  ret i64 %size
  } 
  *)
  let dolphin_rc_get_array_length = 
    let env = mk_env Ll.I64 in 
    let array = Ll.Id (Sym.symbol "array") in 
    let len_ptr = fresh_symbol "len_ptr_" in 
    let size = fresh_symbol "size_" in 
    emit env @@ CfgBuilder.add_insn (Some len_ptr, Ll.Gep (Ll.I64, array, [Ll.IConst64 (-1L)]));
    emit env @@ CfgBuilder.add_insn (Some size, Ll.Load (Ll.I64, Ll.Id len_ptr));
    emit env @@ CfgBuilder.term_block (Ll.Ret (Ll.I64, Some (Ll.Id size)));
    let cfg = CfgBuilder.get_cfg !(env.cfgb) in 
    let fbody = Sym.symbol "dolphin_rc_get_array_length", { fty = ([Ll.Ptr (Ll.I64)], Ll.I64); 
    param = [Sym.symbol "array"]; cfg = cfg} in 
    fbody in 
(*     
  define void @dolphin_rc_set_array_length (i64* %array, i64 %size) {
  %len_ptr = getelementptr i64, i64* %array, i64 -1
  store i64 %size, i64* %len_ptr
  ret void
  } *)
  let dolphin_rc_set_array_length = 
    let env = mk_env Ll.Void in 
    let array = Ll.Id (Sym.symbol "array") in 
    let size = Ll.Id (Sym.symbol "size") in 
    let len_ptr = fresh_symbol "len_ptr_" in 
    emit env @@ CfgBuilder.add_insn (Some len_ptr, Ll.Gep (Ll.I64, array, [Ll.IConst64 (-1L)]));
    emit env @@ CfgBuilder.add_insn (None, Ll.Store (Ll.I64, size, Ll.Id len_ptr));
    emit env @@ CfgBuilder.term_block (Ll.Ret (Ll.Void, None));
    let cfg = CfgBuilder.get_cfg !(env.cfgb) in 
    let fbody = Sym.symbol "dolphin_rc_set_array_length", { fty = ([Ll.Ptr (Ll.I64); Ll.I64], Ll.Void); 
    param = [Sym.symbol "array"; Sym.symbol "size"]; cfg = cfg} in 
    fbody in
(* 
  define i32 @dolphin_rc_compute_array_length_size () {
  %size_ptr = getelementptr i64, i64* null, i64 1
  %size = ptrtoint i64* %size_ptr to i32
  ret i32 %size
  } 
  *)
  let dolphin_rc_compute_array_length_size = 
    let env = mk_env Ll.I32 in 
    let size_ptr = fresh_symbol "size_ptr_" in 
    let size = fresh_symbol "size_" in 
    emit env @@ CfgBuilder.add_insn (Some size_ptr, Ll.Gep (Ll.I64, Ll.Null, [Ll.IConst64 1L]));
    emit env @@ CfgBuilder.add_insn (Some size, Ll.Ptrtoint (Ll.I64, Ll.Id size_ptr, Ll.I32));
    emit env @@ CfgBuilder.term_block (Ll.Ret (Ll.I32, Some (Ll.Id size)));
    let cfg = CfgBuilder.get_cfg !(env.cfgb) in 
    let fbody = Sym.symbol "dolphin_rc_compute_array_length_size", { fty = ([], Ll.I32); 
    param = []; cfg = cfg} in 
    fbody in

  let gdecls = Sym.Table.fold (fun _ v acc -> v :: acc) nstr_map [] in
  let string_type = Ll.Struct [Ll.I64; Ll.Ptr (Ll.I8)] in 
  let str_tp_ptr = Ll.Ptr (Ll.Namedt (Sym.symbol "string_type")) in
  let dolphin_record_type = (Sym.symbol "dolphin_record_stream", Ll.Struct []) in 
  let dolphin_record_type_ptr = Ll.Ptr (Ll.Namedt (Sym.symbol "dolphin_record_stream")) in 
  

  let func_decls = List.fold_left helper [] prog in 
  { tdecls    = (rcd_binding_to_tdecl rcd_bindings) @ [Sym.symbol "string_type", string_type] @ [dolphin_record_type]
  ; extgdecls = []
  ; gdecls    = gdecls
  ; extfuns   = [ (symbol "print_integer",  ([I64], Void))
                ; (symbol "read_integer", ([], I64))
                ; (symbol "allocate_record", ([I32], Ll.Ptr (I8)))
                ; (symbol "report_error_array_index_out_of_bounds", ([], Void))
                ; (symbol "allocate_array", ([I32; I64; Ptr I8], Ptr I8))
                ; (symbol "output_string", ([str_tp_ptr; dolphin_record_type_ptr], Void))
                ; (symbol "get_stdout", ([], dolphin_record_type_ptr))
                ]
  ; fdecls = func_decls @ [dolphin_rc_get_array_length; dolphin_rc_set_array_length; dolphin_rc_compute_array_length_size]
  
  }