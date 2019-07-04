%{
open Syntax
%}

%token LPAREN RPAREN SEMISEMI
%token PLUS SUB MULT EQQ NEQ LT MT AND OR
%token IF THEN ELSE TRUE FALSE
%token LET REC IN EQ RARROW FUN DFUN
%token CORONS ENPBR LBRCT RBRCT SEMI
%token MATCH WITH PARA

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
  | e=AppendExpr { e }
  | e=FunExpr { e }
  | e=MatchExpr { e }
   
AppendExpr :
    l=OrExpr CORONS r=AppendExpr { BinOp(App, l, r) }
  | e=OrExpr { e }

OrExpr :
    l=OrExpr OR r=AndExpr { BinOp(Or, l, r) }
  | e=AndExpr { e }
   
AndExpr :
    l=AndExpr AND r=LTExpr { BinOp(And, l, r) }
  | e=LTExpr { e }
   
LTExpr :
    l=PExpr EQQ r=PExpr { BinOp (Eq, l, r) }
  | l=PExpr NEQ r=PExpr { BinOp (Neq, l, r) }
  | l=PExpr LT r=PExpr { BinOp (Lt, l, r) }
  | l=PExpr MT r=PExpr { BinOp (Mt, l, r) }
  | e=PExpr { e }

PExpr :
    l=PExpr PLUS r=MExpr { BinOp (Plus, l, r) }
  | l=PExpr SUB r=MExpr { BinOp (Sub, l, r) }
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
  | ENPBR { LLit }
  | LBRCT e=ListExpr RBRCT { e }
  | i=ID   { Var i }
  | LPAREN e=Expr RPAREN { e }

ListExpr :
    hd=Expr SEMI tl=ListExpr { BinOp (App, hd, tl) }
  | e=Expr { BinOp (App, e, LLit) }

IfExpr :
    IF c=Expr THEN t=Expr ELSE e=Expr { IfExp (c, t, e) }

LetExpr :   
    LET REC x=ID EQ FUN para=ID RARROW e1=Expr IN e2=Expr { LetRecExp (x, para, e1, e2) }
  | LET x=ID EQ e1=Expr IN e2=Expr { LetExp (x, e1, e2) }

FunExpr :
    FUN x=ID RARROW e=Expr { FunExp (x, e) }
  | DFUN x=ID RARROW e=Expr { DFunExp (x, e) }

MatchExpr :
    MATCH e1=Expr WITH ENPBR RARROW e2=Expr PARA hd=ID CORONS tl=ID RARROW e3=Expr { MatchExp (e1, e2, e3, hd, tl) }



LetsExpr :
    LET REC x=ID EQ FUN p=ID RARROW e=Expr { RecDecl(x, p, e) }
  | LET x=ID EQ e=Expr { Decl(x, e) }
  | l=LetsExpr LET x=ID EQ e=Expr { Decls(x, e, l) }

