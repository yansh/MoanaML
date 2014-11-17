{
open Lexing
open Tuple_parser
}	
rule lex = parse
  | [' ' '\t' '\n']      { lex lexbuf }
  | ","             { COMMA }
  | "("             { LEFT_BRACE }
  | ")"             { RIGHT_BRACE }
  | ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { STRING (s) }
	| '?' ['A'-'Z' 'a'-'z' '_']* as v { VAR (v) }	
  | eof             { EOF }

