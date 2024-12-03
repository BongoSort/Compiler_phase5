(* Env module *)
module TAst = TypedAst
module Sym = Symbol
module Loc = Location

exception Unimplemented (* your code should eventually compile without this exception *)

type varOrFun =
| Var of TAst.typ
| Fun of TAst.funtype
| Rcd of TAst.rcrd

type environment = {vars_and_funs : varOrFun Sym.Table.t;
                   errors : Errors.error list ref;
                   inloop : bool; 
                   fname: string;
                   does_all_branch_ret : bool;
                   ret_type : TAst.typ;
                   func_loc : Loc.location }

(* create an initial environment with the given functions defined *)
let make_env function_types =
  let emp = Sym.Table.empty in
  let env = 
    List.fold_left (fun env (fsym, ftp) -> Sym.Table.add fsym (Fun ftp) env) emp function_types
  in
  {vars_and_funs = env; errors = ref []; inloop = false; fname = ""; does_all_branch_ret = false; ret_type = TAst.ErrorType; func_loc = Loc.dummy_loc}

(* create an initial environment with the given records defined *)
let make_env_rcd rcrd_types =
  let emp = Sym.Table.empty in
  let env = 
    List.fold_left (fun env (fsym, ftp) -> Sym.Table.add fsym (Rcd ftp) env) emp rcrd_types
  in
  {vars_and_funs = env; errors = ref []; inloop = false; fname = ""; does_all_branch_ret = false; ret_type = TAst.ErrorType; func_loc = Loc.dummy_loc}
  
let init_rcrd_env = 
  make_env_rcd RuntimeBindings.library_rcrds 

let extend_rcrd_env (env : environment) rcrd_bindings =
  let helper (acc : varOrFun Sym.Table.t) ((sym, rcrd) : (Sym.symbol * TAst.rcrd)) : varOrFun Sym.Table.t =
    let n_env = Sym.Table.add sym (Rcd rcrd) acc in
    n_env in
  let new_env = 
    List.fold_left helper env.vars_and_funs rcrd_bindings
  in
  {vars_and_funs = new_env; errors = env.errors; inloop = env.inloop; fname = env.fname; does_all_branch_ret = env.does_all_branch_ret; ret_type = env.ret_type; func_loc = env.func_loc}

let extend_func_env (env : environment) rcrd_bindings =
  let helper (acc : varOrFun Sym.Table.t) ((sym, rcrd) : (Sym.symbol * TAst.funtype)) : varOrFun Sym.Table.t =
    let n_env = Sym.Table.add sym (Fun rcrd) acc in
    n_env in
  let new_env = 
    List.fold_left helper env.vars_and_funs rcrd_bindings
  in
  {vars_and_funs = new_env; errors = env.errors; inloop = env.inloop; fname = env.fname; does_all_branch_ret = env.does_all_branch_ret; ret_type = env.ret_type; func_loc = env.func_loc}

let make_env_rcrd rcrd_types = 
  let emp = Sym.Table.empty in
  let env = 
    List.fold_left (fun env (fsym, ftp) -> Sym.Table.add fsym (Rcd ftp) env) emp rcrd_types
  in
  {vars_and_funs = env; errors = ref []; inloop = false; fname = ""; does_all_branch_ret = false; ret_type = TAst.ErrorType; func_loc = Loc.dummy_loc}


let set_in_loop (env: environment) (bol: bool) : environment = 
  {vars_and_funs = env.vars_and_funs; errors = env.errors; inloop = bol; fname = env.fname; does_all_branch_ret = env.does_all_branch_ret; ret_type = env.ret_type; func_loc = env.func_loc}

let set_fname (env: environment) (name : string) : environment = 
  {vars_and_funs = env.vars_and_funs; errors = env.errors; inloop = env.inloop; fname = name; does_all_branch_ret = env.does_all_branch_ret; ret_type = env.ret_type; func_loc = env.func_loc}

let set_all_branch_ret (env: environment) (bol: bool) : environment = 
  {vars_and_funs = env.vars_and_funs; errors = env.errors; inloop = env.inloop; fname = env.fname; does_all_branch_ret = bol; ret_type = env.ret_type; func_loc = env.func_loc}

let set_ret_type (env: environment) (typ: TAst.typ) : environment = 
  {vars_and_funs = env.vars_and_funs; errors = env.errors; inloop = env.inloop; fname = env.fname; does_all_branch_ret = env.does_all_branch_ret; ret_type = typ; func_loc = env.func_loc}

let set_func_loc (env: environment) (loc: Loc.location) : environment = 
  {vars_and_funs = env.vars_and_funs; errors = env.errors; inloop = env.inloop; fname = env.fname; does_all_branch_ret = env.does_all_branch_ret; ret_type = env.ret_type; func_loc = loc}
let get_in_loop (env : environment) : bool = 
  env.inloop


(* insert a local declaration into the environment *)
let insert_local_decl env sym typ =
  let {vars_and_funs; _} = env in
  {env with vars_and_funs = Sym.Table.add sym (Var typ) vars_and_funs}

let insert_error env err =
  let {errors; _} = env in
  errors := err :: !errors

let rec print_err_list err_lst =
  match err_lst with
  | [] -> () (* Error list empty, do nothing *)
  | head::tail -> Errors.print_error head;
    print_err_list tail

let print_all_errors env =
  let {errors; _} = env in
  let err_lst = !errors in
  print_err_list err_lst

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup_var_fun env sym =
  let {vars_and_funs; _} = env in
  Sym.Table.find_opt sym vars_and_funs