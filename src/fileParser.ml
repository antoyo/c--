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

let rec get_all_tokens tokens =
    match Lexer.next_token () with
    | (Lexer.Eof, _) as token -> tokens @ [token]
    | token -> get_all_tokens (tokens @ [token])
    | exception Lexer.UnexpectedCharacter (character, (line, column)) ->
            print_int line;
            print_char ':';
            print_int column;
            print_string ": Unexpected character `";
            print_char character;
            print_string "` on line ";
            print_int line;
            print_endline ".";
            []

let parse filename =
    Lexer.lex filename;
    let tokens = get_all_tokens [] in
    Lexer.close;
    tokens
