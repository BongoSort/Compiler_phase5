open OUnit2
open Phase5
open Env
open Semant
module Err = Errors
module TAst = TypedAst
module Sym = Symbol
module Pretty = Pretty
module TPretty = TypedPretty
module PrintBox_text = PrintBox_text
module Run = RuntimeBindings
(*
(*######################## USEFUL FUNCTIONS ##############################*)
let mk_ident name = Ast.Ident {name}
let mk_int i = Ast.Integer {int = i}
let mk_call fname args = Ast.Call {fname; args}
let mk_binop left op right = Ast.BinOp {left; op; right}

let mk_lval var = Ast.Var (mk_ident var)

let mk_decl var tp body = Ast.VarDeclStm {name = mk_ident var; tp; body}

let mk_ifthenelse cond thbr elbro = Ast.IfThenElseStm {cond; thbr; elbro}

let return_zero = Ast.ReturnStm {ret = mk_int 0L}
let call_print_integer e = Ast.ExprStm {expr = Some (mk_call (mk_ident "print_integer") [e])}

let call_read_integer = mk_call (mk_ident "read_integer") []


(*########################## INT OR BOOL EXPR ###############################*)
(* This is just the number 1 *)
let ast_int_expr_1 = Ast.Integer {int = Int64.one}
(* largest 64 Integer possible *)
let ast_int_expr_2 = Ast.Integer {int = Int64.max_int}

let ast_bool_expr_true = Ast.Boolean {bool = true}

(* infer_empty calls infertype_expr with param1 already set to an empty environment *)
let infer_empty = infertype_expr (Env.make_env [])

(*############################# BINOP #######################################*)

let ast_binop_1_plus_1 = (Ast.BinOp {left = ast_int_expr_1; op = Plus; right = ast_int_expr_1})
let ast_binop_1_le = (Ast.BinOp {left = ast_int_expr_1; op = Le; right = ast_int_expr_1})


(*############################## LVAL #######################################*)
let sym_x = Sym.symbol "x"

(* A non empty environment containing the variable "x" of type Int *)
let non_empty_env = Env.insert_local_decl (Env.make_env []) sym_x TAst.Int
(* A non empty environment containing the variable "x" of type Bool *)
let env_with_x_bool = Env.insert_local_decl (Env.make_env []) sym_x TAst.Bool


(*########################### ASSIGNMENT ###################################*)
let int_lval_expr = (TAst.Lval (Var { ident = (Ident {sym = sym_x}); tp = TAst.Int}))
let bool_lval_expr = (TAst.Lval (Var { ident = (Ident {sym = sym_x}); tp = TAst.Bool}))
let ast_int_lval = Ast.Var (Ident {name = "x"})
let ast_assign_expr = Ast.Assignment {lvl = ast_int_lval; rhs = ast_binop_1_plus_1}
let ast_assign_bool = Ast.Assignment {lvl = ast_int_lval; rhs = ast_binop_1_le}
let int_lval = find_lval_in_expr int_lval_expr
let bool_lval = find_lval_in_expr bool_lval_expr
let tast_binop_1_plus_1 = (TAst.BinOp { left = (TAst.Integer {int = Int64.one}); op = TAst.Plus; right = (TAst.Integer {int = Int64.one}); tp = TAst.Int }), TAst.Int
let tast_binop_1_le = (TAst.BinOp { left = (TAst.Integer {int = Int64.one}); op = TAst.Le; right = (TAst.Integer {int = Int64.one}); tp = TAst.Bool}), TAst.Bool

let tast_binop_1_plus_1_expr = fst tast_binop_1_plus_1
let tast_binop_1_le_expr = fst tast_binop_1_le

(* Lval of x  *)
let ast_lval_expr = Ast.Lval (Var (Ident {name = "x"}))
let x_symbol = Sym.symbol "x"
let env_with_x_bool = Env.insert_local_decl (Env.make_env []) x_symbol TAst.Bool
let my_expr_typ = infertype_expr non_empty_env ast_int_expr_1

(* EMPTY ENVIRONMENT *)

let get_fst_err env =
  let {errors; _} = env in
  let err_lst = !errors in
  match err_lst with
  | [] -> Err.TestError
  | head::_ -> head

(* ALL ERROR TYPES *)

let _ = Printf.printf "\n\n\n"

let _ = print_endline "COMPLETE LIST OF POSSIBLE ERRORS:"
let _ = Errors.print_error (Errors.TypeMismatch {expected = TAst.Int; actual = TAst.ErrorType})
let _ = Errors.print_error (Errors.BinOpTypeMisMatch {expected = TAst.Int; actual = TAst.ErrorType})
let _ = Errors.print_error Errors.MissingReturnStatement
let _ = Errors.print_error Errors.EmptyProgram
let _ = Errors.print_error (Errors.ExpectedVar {expected = "TEST"})
let _ = Errors.print_error (Errors.ExpectedFun {expected = "TEST"})
let _ = Errors.print_error (Errors.ArgLengthNotEqual {fname = "TEST FNAME"; actual = 3; expected = 1})
let _ = Errors.print_error Errors.VoidAssignment
let _ = Errors.print_error Errors.TestError

let _ = Printf.printf "\n\n\n"


(* FAULTY PROGRAMS *)
(* Error: Missing return statement *)
let e = initial_environment()
let faulty_prog_01 = [call_print_integer (mk_int 5L)]
let err_lst_faulty_prog_01 = typecheck_statement_seq e faulty_prog_01
let fst_err_faulty_prog_01 = get_fst_err e
let _ = print_endline "Faulty_prog_01 error list:"
let _ = print_all_errors e
let _ = print_endline ""


(* Error: type mismatch. Lval should only be int or bool *)
(* Error: BinOp type mismatch *)
(* Error: Expected var*)
let e1 = initial_environment()
let ast_lval_y = (Ast.Lval ((Ast.Var (Ast.Ident {name = "y"}))))
let faulty_prog_02 = [mk_decl "x" None ast_lval_y; call_print_integer (mk_binop (mk_int 1L) Ast.Plus (Ast.Lval (mk_lval "x"))); return_zero]
let _ = typecheck_statement_seq e1 faulty_prog_02
let _ = print_endline "Faulty_prog_02 error list:"
let _ = print_all_errors (e1)
let _ = print_endline ""

(* Empty program *)
let e3 = initial_environment()
let faulty_prog_03 =
  []

let _ = typecheck_statement_seq e3 faulty_prog_03
let _ = print_endline "Faulty_prog_03 error list:"
let _ = print_all_errors (e3)
let _ = print_endline ""

(* Error: Expected fun*)
let e4 = initial_environment()
let faulty_prog_04 =
  [
    Ast.ExprStm {expr = Some (mk_call (Ast.Ident {name = "test"}) [])};
    return_zero
  ]

let _ = typecheck_statement_seq e4 faulty_prog_04
let _ = print_endline "Faulty_prog_04 error list:"
let _ = print_all_errors (e4)
let _ = print_endline ""


(* Error: ArgLengthNotEqual*)
let e5 = initial_environment()

let faulty_prog_05 =
  [
    Ast.ExprStm {expr = Some (mk_call (mk_ident "print_integer") [])};
    return_zero
  ]

let _ = typecheck_statement_seq e5 faulty_prog_05
let _ = print_endline "Faulty_prog_05 error list:"
let _ = print_all_errors (e5)
let _ = print_endline ""

(* Error: VoidAssignment *)
let e6 = initial_environment()

let faulty_prog_06 =
  [
    mk_decl "x" None (mk_call (mk_ident "print_integer") [(mk_int 5L)]);
    Ast.ReturnStm {ret = (Ast.Lval (mk_lval "x"))}
  ]

let _ = typecheck_statement_seq e6 faulty_prog_06
let _ = print_endline "Faulty_prog_06 error list:"
let _ = print_all_errors (e6)
let _ = print_endline ""

(* Error: InvalidExpressionStatement *)
let e7 = initial_environment()

let faulty_prog_07 =
  [
    Ast.ExprStm {expr = Some (mk_int 5L)};
    return_zero
  ]

let _ = typecheck_statement_seq e7 faulty_prog_07
let _ = print_endline "Faulty_prog_07 error list:"
let _ = print_all_errors (e7)
let _ = print_endline ""

(* Test suite for infertype_expr *)
let infertype_expr_tests = "test suite for infertype_expr" >::: [
        (* 1 *)
        "IntExp" >:: (fun _ -> assert_equal
                                ((TAst.Integer {int = Int64.one}), TAst.Int)
                                (infer_empty ast_int_expr_1 ));
        (* true  *)
        "BoolExp" >:: (fun _ -> assert_equal
                                ((TAst.Boolean {bool = true}), TAst.Bool)
                                (infer_empty ast_bool_expr_true ));
        (* 1 + 1 *)
        "BinOpExp" >:: (fun _ -> assert_equal
                                ((TAst.BinOp { left = (TAst.Integer {int = Int64.one}); op = TAst.Plus; right = (TAst.Integer {int = Int64.one}); tp = TAst.Int }), TAst.Int)
                                (infer_empty ast_binop_1_plus_1));
        (* x = 1 *)
        "LvalInt" >:: (fun _ -> assert_equal
                                ((TAst.Lval (Var { ident = (Ident {sym = sym_x}); tp = TAst.Int})), TAst.Int)
                                (infertype_expr non_empty_env ast_lval_expr));
        (* x = true *)
        "LvalBool" >:: (fun _ -> assert_equal
                                ((TAst.Lval (Var { ident = (Ident {sym = sym_x}); tp = TAst.Bool})), TAst.Bool)
                                (infertype_expr env_with_x_bool ast_lval_expr));
       
        (* x = 1 + 1 *)
        "Integer Assignment" >::(fun _ -> assert_equal
                                ((TAst.Assignment {lvl = int_lval; rhs = tast_binop_1_plus_1_expr; tp = TAst.Int}), TAst.Int)
                                (infertype_expr non_empty_env ast_assign_expr));

        (* x = (1 < 1) *)
        "Boolean Assignment x defined as int" >::(fun _ -> assert_equal
                                ((TAst.Assignment {lvl = int_lval; rhs = tast_binop_1_le_expr; tp = TAst.ErrorType}), TAst.ErrorType)
                                (infertype_expr non_empty_env ast_assign_bool));
(* Other tests *)

        (* Missing return *)
        "Missing Return Statement" >:: (fun _ -> assert_equal
                                (Err.MissingReturnStatement)
                                (fst_err_faulty_prog_01));
        
                                  ]
let _ = run_test_tt_main infertype_expr_tests
*)
