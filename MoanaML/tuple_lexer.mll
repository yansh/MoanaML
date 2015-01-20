(*
* Copyright (c) 2014 Yan Shvartzshnaider
*
* Permission to use, copy, modify, and distribute this software for any
* purpose with or without fee is hereby granted, provided that the above
* copyright notice and this permission notice appear in all copies.
*
* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)


{
open Lexing
open Tuple_parser

(* code for the fun taken from  Real World Ocaml book *)
exception SyntaxError of string
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}	

let newline = '\r' | '\n' | "\r\n"    

rule lex = parse
  | [' ' '\t' '\n']      { lex lexbuf }
	| newline         { next_line lexbuf; lex lexbuf }
  | ","             { COMMA }
	| "["							{ LEFT_SQUARE_BRACE}
  | "("             { LEFT_BRACE }
	| "{"             {START}
  |['a'-'z' 'A'-'Z' '0'-'9' '_' '@' '.' '-']*[' ']?['a'-'z' 'A'-'Z' '0'-'9' '_' '@' '.' '-']+ as s { STRING (s) }
  | '?' ['A'-'Z' 'a'-'z' '_']* as v { VAR (v) }   	 
  | ")"             { RIGHT_BRACE }
	| "}"             {END}
	| "]"							{RIGHT_SQUARE_BRACE}
	| _               { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof             { EOF }

