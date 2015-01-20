{
open Lexing
open Query_parser

exception SyntaxError of string

(* code for the fun taken from  Real World Ocaml book *)
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

	
}	

let newline = '\r' | '\n' | "\r\n"     
rule lex = parse
  | [' ' '\t']      { lex lexbuf }
	| newline         { next_line lexbuf; lex lexbuf }
  | ","             { COMMA }
	| "MAP" 					{MAP}
	| "{"             {START}
	| ['A'-'Z' 'a'-'z' '_']['0'-'9' 'A'-'Z' 'a'-'z' '_']* as s { STRING (s) }
  | '?' ['A'-'Z' 'a'-'z' '_']* as v { VAR (v) }
	| "}"							{END}
	| _ 							{ raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof             { EOF }

