%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS MULT LT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET IN EQ RARROW FUN DFUN

%token <int> INTV
%token <Syntax.id> ID

%start toplevel
%type <Syntax.program> toplevel
%%

toplevel :
    e=Expr SEMISEMI { Exp e }
  | e=LetsExpr SEMISEMI { e }
   
Expr :
    e=IfExpr { e }
  | e=LetExpr { e }
  | e=AndExpr { e }
  | e=FunExpr { e }

   
AndExpr :
    l=Expr AND r=Expr { BinOp(And, l, r) }
  | e =OrExpr { e }
   
OrExpr :
    l=Expr OR r=Expr { BinOp(Or, l, r) }
  | e = LTExpr { e }
   
LTExpr :
    l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | e=MExpr { e }

MExpr :
    l=MExpr MULT r=AppExpr { BinOp (Mult, l, r) }
  | e=AppExpr { e }

AppExpr :
    e1=AppExpr e2=AExpr { AppExp (e1, e2) }
  | e=AExpr { e }
   
AExpr :
    i=INTV { ILit i }
  | TRUE   { BLit true }
  | FALSE  { BLit false }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :   
    LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }
  | DFUN x=ID RARROW e=Expr { DFunExp (x, e) }
   
LetsExpr :
    LET x=ID EQ e=Expr { Decl(x, e) }
  | l=LetsExpr LET x=ID EQ e=Expr { Decls(x, e, l) }

