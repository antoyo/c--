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
    let (file, line, column) = position in
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

let get_all_tokens filename =
    let token_stream = Lexer.tokens filename in
    let rec get_all_tokens tokens =
        match Stream.next token_stream with
        | token -> get_all_tokens (tokens @ [token])
        | exception Stream.Failure -> tokens
        | exception Lexer.SyntaxError (message, position) ->
                print_error ("Syntax error: " ^ message) position;
                []
        | exception Lexer.UnexpectedCharacter (character, position) ->
                print_error ("Unexpected character `" ^ (Char.escaped character) ^ "`") position;
                []
    in get_all_tokens []

let parse filename =
    let tokens = get_all_tokens filename in
    Lexer.close ();
    tokens
