
(* The type of tokens. *)

type token = 
  | TRUE
  | THEN
  | SEMISEMI
  | RPAREN
  | PLUS
  | OR
  | MULT
  | LT
  | LPAREN
  | INTV of (int)
  | IF
  | ID of (Syntax.id)
  | FALSE
  | ELSE
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val toplevel: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.program)
