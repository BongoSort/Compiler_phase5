module Comp = Phase5.Compile

  (* These are working programs *)


let _ = 
  Comp.compile_prog "dolphinPrograms/dprog30.dlp"


  (* Below is used to easily run progs that should not compile *)
(*
let _ =
  Comp.compile_prog "negative_tests/prog17.dlp"*)

(*
let _ = 
  Comp.compile_prog "prog.dlp"*)
let _ = print_endline ""