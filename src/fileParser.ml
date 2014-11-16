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

let print_error message position =
    let {FileReader.position_filename = file; FileReader.position_line = line; FileReader.position_column = column} = position in
    print_string file;
    print_char ':';
    print_int line;
    print_char ':';
    print_int column;
    print_string ": ";
    print_string message;
    print_string " on line ";
    print_int line;
    print_endline "."

let get_all_tokens lexer =
    let rec get_all_tokens tokens =
        match Lexer.next_token lexer with
        | (Lexer.Eof, _) -> tokens
        | token -> get_all_tokens (tokens @ [token])
        | exception Lexer.SyntaxError {Lexer.error_message; Lexer.error_position} ->
                print_error ("Syntax error: " ^ error_message) error_position;
                []
        | exception Lexer.UnexpectedCharacter {Lexer.error_message; Lexer.error_position} ->
                print_error ("Syntax error: " ^ error_message) error_position;
                []
    in get_all_tokens []

let parse filename =
    let lexer = Lexer.create filename in
    let tokens = get_all_tokens lexer in
    Lexer.close lexer;
    tokens
