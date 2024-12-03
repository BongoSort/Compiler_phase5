(* Errors module *)
module Sym = Symbol
module TAst = TypedAst
module TPretty = TypedPretty
module Loc = Location

type error =
| TypeMismatch of {expected : TAst.typ; actual : TAst.typ; loc : Loc.location}
| MissingReturnStatement of { fname :  string; loc : Loc.location }
| ExpectedFun of {expected : string; loc : Loc.location}
| ExpectedVar of {expected : string; loc : Loc.location}
| ArgLengthNotEqual of {fname : string; actual : int; expected : int; loc : Loc.location}
| BinOpTypeMisMatch of {expected : TAst.typ; actual : TAst.typ; loc : Loc.location}
| VoidAssignment of { loc : Loc.location }
| EmptyProgram of { loc : Loc.location }
| InvalidExpressionStatement of { loc : Loc.location }
| NotInLoop of { loc : Loc.location }
| TestError of { loc : Loc.location }
| DublicateParameter of {id : string; loc : Loc.location }
| MissingMainMethod  
| MainHasParameters of {loc : Loc.location }
| NotUniqueMethodName of {loc : Loc.location }
| MainHasWrongRetType of {loc : Loc.location }
| RetExprInVoid of {loc : Loc.location }
| MissingRetExpr of {fname : string; loc : Loc.location } 
| DuplicateRecordDecleration of {rname : string; loc : Loc.location}
| StdLibraryRecprd of {rname : string; loc : Loc.location}
| ReferringToNonExistingRcrd of {rname : string; fieldname : string; loc : Loc.location}
| DuplicateField of {fieldname : string; loc : Loc.location}
| RecordMisMatch of {expected : TAst.typ; loc : Loc.location}
| ExprNotARecord of {loc : Loc.location}
| FieldDoesNotExist of {fname : string; loc : Loc.location}
| ExpectedRcrd of {rname : string; loc : Loc.location}
| ArrayExprNotInt of {loc : Loc.location}
| ExprNotAnArray of {loc : Loc.location}

(* other errors to be added as needed. *)

(* Useful for printing errors *)
let error_to_string err =
  match err with
  | TypeMismatch {expected; actual; loc} -> Printf.sprintf "Type mismatch: expected %s but found %s. %s." 
  (TPretty.typ_to_string expected) (TPretty.typ_to_string actual) (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | BinOpTypeMisMatch {expected; actual; loc} -> Printf.sprintf "BinOp type mismatch: left has type %s right has type %s. %s." 
  (TPretty.typ_to_string expected) (TPretty.typ_to_string actual) (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | MissingReturnStatement {fname; loc} -> Printf.sprintf"The function %s does not end in a return statement. %s."
    fname (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | EmptyProgram {loc} -> Printf.sprintf"The program is empty. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | ExpectedVar {expected; loc} -> Printf.sprintf"'%s' was expected to be a var. Maybe it has not been declared. %s." expected 
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | ExpectedFun {expected; loc} -> Printf.sprintf"'%s' was expected to be a function. Maybe it has not been declared. %s." expected
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | ArgLengthNotEqual {fname; actual; expected; loc} -> Printf.sprintf"%s expected %d arguments, but got %d. %s." fname expected actual
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | VoidAssignment {loc} -> Printf.sprintf"Void assignment/declaration is not valid. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | InvalidExpressionStatement {loc}-> Printf.sprintf"Expression statemtent must be a function call or assignment. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | NotInLoop {loc} -> Printf.sprintf "Break/Continue must be in a loop. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | TestError {loc} -> Printf.sprintf "TEST ERROR. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | DublicateParameter {id; loc } -> Printf.sprintf "Multiple %s in parameter. %s."
    id (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | MissingMainMethod -> Printf.sprintf "Missing main method."
  | MainHasParameters {loc} -> Printf.sprintf "Main method should not take any parameters. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | NotUniqueMethodName {loc} -> Printf.sprintf "The method name is not unique. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
  | MainHasWrongRetType {loc} -> Printf.sprintf "Main should return int. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| RetExprInVoid {loc} -> Printf.sprintf "Not able to return an expression in void. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| MissingRetExpr {fname; loc} -> Printf.sprintf "%s is missing expression in return statement. %s."
    fname (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| DuplicateRecordDecleration {rname; loc} -> Printf.sprintf "%s is declared multiple times. %s."
    rname (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| StdLibraryRecprd {rname; loc} -> Printf.sprintf "%s is a std library function and can hence not be user defined. %s."
    rname (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| ReferringToNonExistingRcrd {rname; fieldname; loc} -> Printf.sprintf "%s is mapping to a non existing record : %s. %s."
    fieldname rname (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| DuplicateField {fieldname; loc} -> Printf.sprintf "%s is a duplicate field, which is not allowed. %s."
    fieldname (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| RecordMisMatch {expected; loc} -> Printf.sprintf "%s was expected to be a record, but the record doesn`t exist. %s."
    (TPretty.typ_to_string expected) (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| ExprNotARecord {loc} -> Printf.sprintf "The type of the expression was expected to be a record but was something else %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| FieldDoesNotExist{fname; loc} -> Printf.sprintf "The field %s does not exist. %s."
    fname (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| ExpectedRcrd {rname; loc} -> Printf.sprintf "The record %s does not exist. %s."
    rname (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| ArrayExprNotInt {loc} -> Printf.sprintf "The expression in the array declaration was expected to be an int, but was something else. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)
| ExprNotAnArray {loc} -> Printf.sprintf "The expression was expected to be an array, but was something else. %s."
    (PrintBox_text.to_string @@ Loc.location_to_tree loc)

(* Prints an error *)
  


let print_error err =
  let err_string = error_to_string err in
  print_endline err_string