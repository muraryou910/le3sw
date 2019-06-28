{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("let", Parser.LET);
  ("rec", Parser.REC);
  ("in", Parser.IN);
  ("fun", Parser.FUN);
  ("dfun", Parser.DFUN);
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| ";" { Parser.SEMI }
| "->" { Parser.RARROW }
| "+" { Parser.PLUS }
| "-" { Parser.SUB }
| "*" { Parser.MULT }
| "==" { Parser.EQQ }
| "<>" { Parser.NEQ }
| "<" { Parser.LT }
| ">" { Parser.MT }
| "&&" { Parser.AND }
| "||" { Parser.OR }
| "=" { Parser.EQ }
| "|" { Parser.PARA }
| "::" { Parser.CORONS }
| "[]" { Parser.ENPBR }
| "[" { Parser.LBRCT }
| "]" { Parser.RBRCT }

| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }


