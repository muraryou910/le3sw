open Syntax

type exval =
    IntV of int
  | BoolV of bool
  | ProcV of id * exp * dnval Environment.t
  | DProcV of id * exp
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
    IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (x, v, env) -> "<fun>"
  | DProcV (x,v) -> "<dfun>"

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
    Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +")
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | And, BoolV v1, BoolV v2 -> BoolV (v1 && v2)
  | And, _, _ -> err ("Both arguments must be bool: &&")
  | Or, BoolV v1, BoolV v2 -> BoolV (v1 || v2)
  | Or, _, _ -> err ("Both arguments must be bool: ||")

let rec eval_exp env = function
    Var x ->
    (try Environment.lookup x env with
       Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) ->
    let arg1 = eval_exp env exp1 in
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
    (match test with
       BoolV true -> eval_exp env exp2
     | BoolV false -> eval_exp env exp3
     | _ -> err ("Test expression must be boolean: if"))
  | LetExp (id, exp1, exp2) ->
     let value = eval_exp env exp1 in
     eval_exp (Environment.extend id value env) exp2
  | FunExp (id, exp) -> ProcV (id, exp, env)
  | DFunExp (id,exp) -> DProcV (id, exp)
  | AppExp (exp1, exp2) ->
     let funval = eval_exp env exp1 in
     let arg = eval_exp env exp2 in(
     match funval with
       ProcV (id, body, env') -> let newenv = Environment.extend id arg env' in
				 eval_exp newenv body
     | DProcV (id, body) -> let newenv = Environment.extend id arg env in
			    eval_exp newenv body
     | _ -> err ("Non-function value is applied"))

let for_decls env id1 e1 id2 env2 v2 =
  Printf.printf "val %s = %s\n" id2 (string_of_exval v2);
  let env1 = Environment.extend id2 v2 env2 in
  let v1 = eval_exp env1 e1 in (id1, Environment.extend id1 v1 env2, v1)
       
let rec eval_decl env = function
    Exp e -> let v = eval_exp env e in ("-", env, v)
  | Decl (id, e) ->
     let v = eval_exp env e in (id, Environment.extend id v env, v)
  | Decls (id, e, dec) ->
     let (decid, decenv, decv) = eval_decl env dec in for_decls env id e decid decenv decv
  
  






