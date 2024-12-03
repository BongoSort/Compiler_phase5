module Ast = Ast 
module Sem = Semant
module Cg = CodeGen
module Env = Env
module Parser = Parser
module Lexer = LexDolphin
module Pretty = Pretty

(*
let compile_prog_print (prog : Ast.statement list) : unit = 
  let in_env = Sem.initial_environment() in 
  let tprog = Sem.typecheck_statement_seq in_env prog in 
  let pot_err = !(in_env.errors) in 
  match pot_err with
  | _::_ -> 
    Env.print_all_errors in_env
  | _ -> 
    let llvm = Cg.trans_prog tprog in 
    let str = Ll.string_of_prog llvm in 
    print_endline str;
  *)


let compile_prog_to_file (prog : Ast.program) (name : string) : unit =
  let tprog, in_env = Sem.typecheck_function_seq prog in 
  let pot_err = !(in_env.errors) in 
  match pot_err with
  | _::_ -> 
    let _ = Env.print_all_errors in_env in 
    print_endline "Failed to compile to a file.";
  | _ -> 
    let llvm = Cg.trans_prog tprog in 
    let str = Ll.string_of_prog llvm in 
    let oc = open_out name in
    let _ = output_string oc str in 
    close_out oc;
    print_endline "success compiled to a file."
  
let compile_prog (name : string) =
  let file_in = open_in name in 
  let lex_buf = Lexing.from_channel file_in in 
  try
  let prog = Parser.program_list Lexer.token lex_buf in
  let compiled_name = name ^ ".ll" in
  compile_prog_to_file prog compiled_name
  with 
  | Lexer.Unrecognized -> print_endline "There was a problem lexing file."
  | CodeGen.NotPossible -> print_endline "gotcha bitch."
  | _ as e -> print_endline "There was a problem parsing the tokens.";
  print_endline @@ Printexc.to_string e;
