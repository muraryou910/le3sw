(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Sub | Mult | Eq | Neq | Lt | Mt | And | Or | App

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | LLit
  | ILLit of int list
  | BLLit of bool list
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | LetRecExp of id * id * exp * exp
  | FunExp of id * exp
  | DFunExp of id * exp
  | AppExp of exp * exp
  | MatchExp of exp * exp * exp * id * id

      
type program =
    Exp of exp
  | Decl of id * exp
  | Decls of id * exp * program
  | RecDecl of id * id * exp

type tyvar = int
type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty


