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

open Lexer
open Lexing

let print_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    print_string pos.pos_fname;
    print_string ":";
    print_int pos.pos_lnum;
    print_string ":";
    print_int (pos.pos_cnum - pos.pos_bol + 1);
    print_string ": "

let parse_with_error lexbuf =
    try Parser.prog Lexer.read lexbuf with
    | SyntaxError msg ->
        print_position lexbuf;
        print_endline msg;
        []
    | Parser.Error ->
        print_string "Syntax error: ";
        print_position lexbuf;
        print_endline "";
        exit (-1)

let parse filename =
    let inx = open_in filename in
    let lexbuf = Lexing.from_channel inx in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let definitions = parse_with_error lexbuf in
    close_in inx;
    definitions
