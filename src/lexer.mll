(*
 * Copyright (C) 2014  Boucher, Antoni <bouanto@gmail.com>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

{
    open Lexing
    open Parser

    exception SyntaxError of string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with
            pos_bol = lexbuf.lex_curr_pos;
            pos_lnum = pos.pos_lnum + 1;
        }
}

let int = '-' ? ['0'-'9'] ['0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
    | white { read lexbuf }
    | newline { next_line lexbuf; read lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | '{' { LEFT_CURLY_BRACKET }
    | '}' { RIGHT_CURLY_BRACKET }
    | '(' { LEFT_PARENTHESIS }
    | ')' { RIGHT_PARENTHESIS }
    | '[' { LEFT_SQUARE_BRACKET }
    | ']' { RIGHT_SQUARE_BRACKET }
    | '*' { STAR }
    | ';' { SEMI_COLON }
    | ',' { COMMA }
    | "==" { EQUALS }
    | '>' { GREATER }
    | ">=" { GREATER_OR_EQUAL }
    | '=' { EQUAL }
    | "++" { PLUS_PLUS }
    | '"' { read_string (Buffer.create 17) lexbuf }
    | "//" { skip_comment lexbuf; read lexbuf }
    | "/*" { skip_multiline_comment lexbuf; read lexbuf }
    | "else" { ELSE }
    | "if" { IF }
    | "const" { CONSTANT }
    | "return" { RETURN }
    | id { ID (Lexing.lexeme lexbuf) }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof { EOF }
and read_string buf = parse
    | '"' { STRING (Buffer.contents buf) }
    | '\\' 'n' { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | [^ '\\' '"']+ {
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf;
    }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError "String is not terminated") }
and skip_multiline_comment = parse
    | newline { next_line lexbuf; skip_multiline_comment lexbuf }
    | "*/" { }
    | _ { skip_multiline_comment lexbuf }
    | eof { raise (SyntaxError "Comment is not terminated") }
and skip_comment = parse
    | newline | eof { next_line lexbuf }
    | _ { skip_comment lexbuf }
