open Phase5
module TAst = TypedAst
module TPretty = TypedPretty
module Sem = Semant
module Sym = Symbol

let pretty_print_program prog =
  PrintBox_text.output stdout (Pretty.program_to_tree prog); output_string stdout "\n"

let pretty_print_expr expr =
  PrintBox_text.output stdout (Pretty.expr_to_tree expr); output_string stdout "\n"

let tast_pretty_print_expr expr =
  PrintBox_text.output stdout (TPretty.expr_to_tree expr); output_string stdout "\n"

let tast_pretty_print_program prog =
  PrintBox_text.output stdout (TPretty.program_to_tree prog); output_string stdout "\n"

 (* TODO cleanup dolphin.ml*)
 
 (* let type_check_program prog =
  let tprog = Sem.typecheck_prog prog in
  PrintBox_text.output stdout (TPretty.program_to_tree tprog);
  output_string stdout "\n" *)

(*
let compile_program prog =
  try
    let tprog = Sem.typecheck_prog prog in
    let llprog = CodeGen.trans_prog tprog in
    output_string stdout (Ll.string_of_prog llprog)
  with
  | Errors.TypeError err -> Printf.eprintf "%s \n" (Errors.error_to_string err)

*)
(* Examples *)


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

(* print_integer(5); return 0; *)
let _prog_01 = [call_print_integer (mk_int 5L); return_zero]

(* print_integer(1 + 5); return 0; *)
let _prog_02 = [call_print_integer (mk_binop (mk_int 1L) Ast.Plus (mk_int 5L)); return_zero]

(* var x = 10; print_integer(1 + x); return 0; *)
let _prog_03 = [mk_decl "x" None (mk_int 10L); call_print_integer (mk_binop (mk_int 1L) Ast.Plus (Ast.Lval (mk_lval "x"))); return_zero]

(* var x = read_integer(); print_integer(1 + x); return 0; *)
let _prog_04 = [mk_decl "x" None (call_read_integer); call_print_integer (mk_binop (mk_int 1L) Ast.Plus (Ast.Lval (mk_lval "x"))); return_zero]

(* var x = read_integer(); if(x < 100) print_integer(1 + x); return 0; *)
let _prog_05 =
[ mk_decl "x" None (call_read_integer);
  mk_ifthenelse
    (mk_binop (Ast.Lval (mk_lval "x")) Ast.Le (mk_int 100L))
    (call_print_integer (mk_binop (mk_int 1L) Ast.Plus (Ast.Lval (mk_lval "x"))))
    None;
  return_zero]

(* var x = read_integer(); if(x < 100) print_integer(1 + x) else print_integer(-1); return 0; *)
let _prog_06 =
  [ mk_decl "x" None (call_read_integer);
    mk_ifthenelse
      (mk_binop (Ast.Lval (mk_lval "x")) Ast.Le (mk_int 100L))
      (call_print_integer (mk_binop (mk_int 1L) Ast.Plus (Ast.Lval (mk_lval "x"))))
      (Some (call_print_integer (mk_int (-1L))));
    return_zero]
    


(* ###### OLIVERS FAULTY PROGRAMS ####### *)
(* Missing return statement *)
let faulty_prog_01 = [call_print_integer (mk_int 5L)]
let e = Sem.initial_environment()
let err_lst_faulty_prog_01 = Sem.typecheck_statement_seq e faulty_prog_01

let _ = Env.print_all_errors e
let _ = print_endline "###########################################"









(*
(* let prog_03 = [Ast.ExprStm {expr = Some (mk_call (mk_ident "print_integer") [mk_binop (mk_int 1L) Ast.Plus (mk_int 5L)])}; Ast.ReturnStm {ret = Some (mk_int 0L)}] (* print_integer(1 + 5); return 0; *)
let expression_03 = BinOp (Mul, BinOp (Add, Int 2, Int 2), Int 2) (* (2+2)*2 *)

let expression_04 = BinOp (Add, Int 2, BinOp (Mul, Int 2, Int 2)) (* 2+2*2   *)

let expressions = [ expression_01
                  ; expression_02
                  ; expression_03
                  ; expression_04
                  ]

let prog_00 : eprog = ([], Int 42)
let prog_01 : eprog =
      ([ Val ("x", expression_01) ] , Var "x")


let prog_02 : eprog =
      ( [ Input ("x") ]
      , BinOp (Add, Int 1, Var "x"))

let prog_03 : eprog =
      ( [ Input ("x") ]
            , BinOp (Div, Int 10, Var "x"))

let prog_04 : eprog =
      ( [ Input "x"
        ; Val ("y", BinOp(Add, Var "x", Int 100))
        ; Input "z" ]
            , BinOp (Add, Var "y", Var "x"))

let prog_05: eprog =
      [ Input "x"
      ; Input "xx"
      ; Input "y"]
         , Int 1

let prog_06: eprog = (
      [ Input "x"
      ; Val ("y", BinOp (Add, Var "x", Int 1))
      ],
      BinOp (Add, Var "x", Var "y")) *)

let () = compile_program _prog_06

let () = ignore pretty_print_program; ignore type_check_program; ignore compile_program *)
(*
let () = pretty_print_program _prog_01
*)
let sym_x = Sym.symbol "x"
let test_expr1 = Ast.Integer {int = Int64.one}

let tast_test_1 = ((TAst.Lval (Var { ident = (Ident {sym = sym_x}); tp = TAst.Int})), TAst.Int)
let tast_test_2 = TAst.Lval (Var { ident = (Ident {sym = sym_x}); tp = TAst.Int})

let _ = tast_pretty_print_expr tast_test_2

let non_empty_env = Env.insert_local_decl (Env.make_env []) sym_x TAst.Int
let infer_with_int_x = Semant.infertype_expr non_empty_env
let ast_int_expr = Ast.Lval (Var (Ident {name = "x"}))

let ast_call_prog = [mk_decl "x" None (call_read_integer); call_print_integer (mk_binop (mk_int 1L) Ast.Plus (Ast.Lval (mk_lval "x"))); return_zero]


let my_tast_2 = fst (Sem.infertype_expr non_empty_env ast_int_expr)


(*let assignment_test_tast = (TAst.Assignment {lvl = int_lval; rhs = tast_binop_1_plus_1_expr; tp = TAst.Int}), TAst.Int)*)

(*
   
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


*)

(* ############################################################## *)
(* Examples for codegen *)
(*
var x = 5 + 3
var y = x + 3
return y
*)
let codegen_01 : Ast.statement list = 
  [
  mk_decl "x" None (mk_binop (mk_int 5L) Ast.Plus (mk_int 3L));
  mk_decl "y" None (mk_binop (Ast.Lval (mk_lval "x")) Ast.Plus (mk_int 3L));
  Ast.ReturnStm {ret = (Ast.Lval (mk_lval "y"))}
  ]

(*
var x = 5 + 3
var y = 7 + 3
return y
*)
let codegen_01_b : Ast.statement list = 
  [
  mk_decl "x" None (mk_binop (mk_int 5L) Ast.Plus (mk_int 3L));
  mk_decl "y" None (mk_binop (mk_int 7L) Ast.Plus (mk_int 3L));
  Ast.ReturnStm {ret = (mk_int 5L)}
  ]
  (*
return 3-10
*)
let codegen_02 : Ast.statement list =
  [
    Ast.ReturnStm {ret = (mk_binop (mk_int 3L) Ast.Minus (mk_int 10L))}
  ]

(*
  x = 5
  if 1 = 1 then Var x = 10 else Var x = 20
  return x   
*)
let codegen_03 : Ast.statement list =
  [ 
    mk_decl "x" None (mk_int 5L);
    mk_ifthenelse (mk_binop (mk_int 1L) Ast.Eq (mk_int 1L)) (mk_decl "x" None (mk_int 10L)) (Some(mk_decl "x" None (mk_int 20L)));
    Ast.ReturnStm {ret = Ast.Lval (mk_lval "x")}
  ]
(*
  var x = 5
  if 2 < 4
    var x = 10 //newvar
    return x
  else 
    return x
*)
let codegen_04 : Ast.statement list =
  [
    mk_decl "x" None (mk_int 5L);
    mk_ifthenelse (mk_binop (mk_int 2L) Ast.Lt (mk_int 4L)) (Ast.CompoundStm {stms = [mk_decl "x" None (mk_int 10L);Ast.ReturnStm {ret = (Ast.Lval (mk_lval "x"))}]}) (Some (Ast.ReturnStm {ret = (Ast.Lval (mk_lval "x"))}));
    return_zero
  ]
(* 
return 10/2   
*)
let codegen_05 : Ast.statement list =
  [
    Ast.ReturnStm {ret = (mk_binop (mk_int 10L) Ast.Div (mk_int 2L))}
  ]
(* 
x = true
y = !x
return 0   
*)
let codegen_06 : Ast.statement list =
  [
    mk_decl "x" None (Ast.Boolean {bool = true});
    mk_decl "y" None (Ast.UnOp{op = Ast.Lnot; operand = (Ast.Lval (mk_lval "x"))});
    return_zero
  ]

(*
  if 5 < 6 
    then {return 3}
  return 0   
*)

let codegen_07 : Ast.statement list =
  [
    mk_ifthenelse (mk_binop (mk_int 5L) Ast.Lt (mk_int 6L)) (Ast.ReturnStm {ret = (mk_int 3L)}) (None);
    return_zero
  ]

(*
  x = 5
  if 1 = 1 then {x = 20; y = x}
  return x   
*)
let codegen_08 : Ast.statement list =
  [
    mk_decl "x" None (mk_int 5L);
    mk_ifthenelse (mk_binop (mk_int 1L) Ast.Eq (mk_int 1L)) (Ast.CompoundStm {stms = [(mk_decl "x" None (mk_int 20L)); (mk_decl "y" None (Ast.Lval (mk_lval "x")))]}) None;
    Ast.ReturnStm {ret = (Ast.Lval (mk_lval "x"))}
  ]

(*THIS SHOULD GIVE VOID ERROR *)
let faulty_prog : Ast.statement list =
  [
    mk_decl "x" None (Ast.Call {fname = (Ast.Ident {name = "print_integer"}); args = [mk_int 4L]});
    return_zero
  ]

(*
  var x = 5
  var x = 10
  return x   
*)
let codegen_10 : Ast.statement list =
  [
    mk_decl "x" None (mk_int 5L);
    mk_decl "x" None (mk_int 10L);
    Ast.ReturnStm {ret = (Ast.Lval (mk_lval "x"))}
  ]

(*
  if (3 < 2 || 4 < 6) 
    then {
      print 4
    } 
  return 0   
*)
let codegen_11 : Ast.statement list =
  [
    mk_ifthenelse (mk_binop (mk_binop (mk_int 3L) Ast.Lt (mk_int 2L)) Ast.Lor (mk_binop (mk_int 4L) Ast.Lt (mk_int 6L)))
                  (Ast.ExprStm {expr = Some (Ast.Call {fname = (Ast.Ident {name = "print_integer"}); args = [mk_int 4L]})}) None;
    return_zero
  ]

(*
  if 3 < 2 && 4 < 6 
    then {
      print(4)
    }   
  return 0
*)
let codegen_12 : Ast.statement list =
  [
    mk_ifthenelse (mk_binop (mk_binop (mk_int 3L) Ast.Lt (mk_int 2L)) Ast.Land (mk_binop (mk_int 4L) Ast.Lt (mk_int 6L)))
                  (Ast.ExprStm {expr = Some (Ast.Call {fname = (Ast.Ident {name = "print_integer"}); args = [mk_int 4L]})}) None;
    return_zero
  ]

(*
  if 3 < 2 || 4 < 3
    then {
      print_integer(4)
    }
  return 0
*)
let codegen_13 : Ast.statement list =
  [
    mk_ifthenelse (mk_binop (mk_binop (mk_int 3L) Ast.Lt (mk_int 2L)) Ast.Lor (mk_binop (mk_int 4L) Ast.Lt (mk_int 3L)))
                  (Ast.ExprStm {expr = Some (Ast.Call {fname = (Ast.Ident {name = "print_integer"}); args = [mk_int 4L]})}) None;
    return_zero
  ]



(*
  if 3 < 4 && 4 < 6
    then {
      print_integer(4)
    }
  return 0
*)
let codegen_14 : Ast.statement list =
  [
    mk_ifthenelse (mk_binop (mk_binop (mk_int 3L) Ast.Lt (mk_int 4L)) Ast.Land (mk_binop (mk_int 4L) Ast.Lt (mk_int 6L)))
                  (Ast.ExprStm {expr = Some (Ast.Call {fname = (Ast.Ident {name = "print_integer"}); args = [mk_int 4L]})}) None;
    return_zero
  ]

(*
  x = 5
  x = 2
  return x
   
*)
let codegen_15 : Ast.statement list =
  [
    mk_decl "x" None (mk_int 5L);
    Ast.ExprStm {expr = Some (Ast.Assignment {lvl = (mk_lval "x"); rhs = (mk_int 2L)})};
    Ast.ReturnStm {ret = (Ast.Lval (mk_lval "x"))}
  ]

  (*
  y = 2
  x = y = 3
  return x
  *)
let codegen_16 : Ast.statement list =
  [
    mk_decl "y" None (mk_int 2L);
    mk_decl "x" None (Ast.Assignment {lvl = (mk_lval "y"); rhs = (mk_int 3L)});
    Ast.ReturnStm {ret = (Ast.Lval (mk_lval "x"))}
  ]
(* Print llvm code for codegen examples *)
let empty_env : Env.environment = Semant.initial_environment()

(*Program for code to be generated for*)
let _ = print_endline "###########################################"
(*let code : Ast.program = codegen_05
let tcode = Semant.typecheck_statement_seq empty_env code
let _ = tast_pretty_print_program tcode
let generated_code = CodeGen.trans_prog tcode
let string_of_code = Ll.string_of_prog generated_code

let _ = print_endline string_of_code;*)
let _ = Compile.compile_prog codegen_04