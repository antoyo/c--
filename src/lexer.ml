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

(*
 * Remember line number and column number in token.
 *)

let eof = char_of_int 4

type file_position = int * int

exception UnexpectedCharacter of char * file_position

type token =
    | Character of char * file_position
    | Colon of file_position
    | Comma of file_position
    | Comment
    | Divide of file_position
    | DivideEqual of file_position
    | Eof of file_position
    | Equal of file_position
    | Float of float * file_position
    | Greater of file_position
    | GreaterOrEqual of file_position
    | Identifier of string * file_position
    | Int of int * file_position
    | IsEqual of file_position
    | LeftCurlyBracket of file_position
    | LeftParenthesis of file_position
    | LeftSquareBracket of file_position
    | Lesser of file_position
    | LesserOrEqual of file_position
    | Minus of file_position
    | MinusEqual of file_position
    | Modulo of file_position
    | ModuloEqual of file_position
    | Not of file_position
    | NotEqual of file_position
    | Plus of file_position
    | PlusEqual of file_position
    | RightCurlyBracket of file_position
    | RightParenthesis of file_position
    | RightSquareBracket of file_position
    | SemiColon of file_position
    | String of string * file_position
    | Times of file_position
    | TimesEqual of file_position

let character c () = Character (c, FileReader.file_position ())
let colon () = Colon (FileReader.file_position ())
let comma () = Comma (FileReader.file_position ())
let comment () = Comment
let divide () = Divide (FileReader.file_position ())
let divideEqual () = DivideEqual (FileReader.file_position ())
let eof () = Eof (FileReader.file_position ())
let equal () = Equal (FileReader.file_position ())
let floating number () = Float (number, FileReader.file_position ())
let greater () = Greater (FileReader.file_position ())
let greaterOrEqual () = GreaterOrEqual (FileReader.file_position ())
let identifier id () = Identifier (id, FileReader.file_position ())
let integer number () = Int (number, FileReader.file_position ())
let isEqual () = IsEqual (FileReader.file_position ())
let leftCurlyBracket () = LeftCurlyBracket (FileReader.file_position ())
let leftParenthesis () = LeftParenthesis (FileReader.file_position ())
let leftSquareBracket () = LeftSquareBracket (FileReader.file_position ())
let lesser () = Lesser (FileReader.file_position ())
let lesserOrEqual () = LesserOrEqual (FileReader.file_position ())
let minus () = Minus (FileReader.file_position ())
let minusEqual () = MinusEqual (FileReader.file_position ())
let modulo () = Modulo (FileReader.file_position ())
let moduloEqual () = ModuloEqual (FileReader.file_position ())
let not_op () = Not (FileReader.file_position ())
let notEqual () = NotEqual (FileReader.file_position ())
let plus () = Plus (FileReader.file_position ())
let plusEqual () = PlusEqual (FileReader.file_position ())
let rightCurlyBracket () = RightCurlyBracket (FileReader.file_position ())
let rightParenthesis () = RightParenthesis (FileReader.file_position ())
let rightSquareBracket () = RightSquareBracket (FileReader.file_position ())
let semiColon () = SemiColon (FileReader.file_position ())
let string_literal value () = String (value, FileReader.file_position ())
let times () = Times (FileReader.file_position ())
let timesEqual () = TimesEqual (FileReader.file_position ())

let keywords = []

let close () =
    FileReader.close_file ()

let if_match_after character result_if_true result_if_false =
    FileReader.next_char ();
    if FileReader.get_char () = character then
        result_if_true
    else (
        FileReader.previous_char ();
        result_if_false
    )

let rec skip_block_comment () =
    FileReader.next_char ();
    match FileReader.get_char () with
    | '*' ->
            FileReader.next_char ();
            (match FileReader.get_char () with
            | '/' -> FileReader.next_char ()
            | _ -> skip_block_comment ())
    | _ -> skip_block_comment ()

let rec skip_line_comment () =
    FileReader.next_char ();
    match FileReader.get_char () with
    | '\n' -> ()
    | _ -> skip_line_comment ()

let get_arithmetic_or_assignment_operator_or_skip_comment () =
    match FileReader.get_char () with
    | '+' -> if_match_after '=' plusEqual plus
    | '-' -> if_match_after '=' minusEqual minus
    | '*' -> if_match_after '=' timesEqual times
    | '/' ->
            FileReader.next_char ();
            (match FileReader.get_char () with
            | '/' -> skip_line_comment (); comment
            | '*' -> skip_block_comment (); comment
            | '=' -> divideEqual
            | _ ->
                FileReader.previous_char ();
                divide
            )
    | '%' -> if_match_after '=' moduloEqual modulo
    | _ -> raise (Invalid_argument "Invalid character")

let get_comparison_or_logical_operator () =
    match FileReader.get_char () with
    | '=' -> if_match_after '=' isEqual equal
    | '<' -> if_match_after '=' lesserOrEqual lesser
    | '>' -> if_match_after '=' greaterOrEqual greater
    | '!' -> if_match_after '=' notEqual not_op
    | _ -> raise (Invalid_argument "Invalid character")

let int_of_digit digit =
    let zero = int_of_char '0' in
    let code = int_of_char digit in
    code - zero

let get_decimals () =
    let rec get_decimals () =
        match FileReader.get_char () with
        | '0' .. '9' ->
                FileReader.next_char ();
                get_decimals ()
        | _ ->
                FileReader.previous_char ()
    in get_decimals ()

let get_exponent () =
    FileReader.next_char ();
    match FileReader.get_char () with
    | 'e' | 'E' ->
            FileReader.next_char ();
            get_decimals ()
    | _ -> FileReader.previous_char ()

let get_number () =
    let rec get_number () =
        match FileReader.get_char () with
        | '0' .. '9' ->
                FileReader.next_char ();
                get_number ()
        | '.' ->
                FileReader.next_char ();
                get_decimals ();
                get_exponent ();
                floating (float_of_string (FileReader.substring ()))
        | 'e' | 'E' ->
                FileReader.next_char ();
                get_decimals ();
                floating (float_of_string (FileReader.substring ()))
        | _ ->
                FileReader.previous_char ();
                integer (int_of_string (FileReader.substring ()))
    in get_number ()

let get_identifier () =
    FileReader.next_char ();
    let rec get_identifier () =
        match FileReader.get_char () with
        | '_' | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'->
                FileReader.next_char ();
                get_identifier ()
        | _ ->
                FileReader.previous_char ();
                identifier (FileReader.substring ())
    in get_identifier ()

let get_string () =
    FileReader.next_char ();
    FileReader.adjust_start_position ();
    let rec get_string () =
        match FileReader.get_char () with
        | '"' ->
                FileReader.previous_char ();
                let token = string_literal (FileReader.substring ()) in
                FileReader.next_char ();
                token
        | _ ->
                FileReader.next_char ();
                get_string ()
    in get_string ()

let get_character () =
    FileReader.next_char ();
    let token = character (FileReader.get_char ()) in
    FileReader.next_char ();
    token

let lex filename =
    FileReader.open_file filename

let rec next_token () =
    FileReader.adjust_start_position ();
    let token = (match FileReader.get_char () with
    | exception End_of_file -> (eof ())
    | '{' -> (leftCurlyBracket ())
    | '}' -> (rightCurlyBracket ())
    | '(' -> (leftParenthesis ())
    | ')' -> (rightParenthesis ())
    | '[' -> (leftSquareBracket ())
    | ']' -> (rightSquareBracket ())
    | ':' -> (colon ())
    | ';' -> (semiColon ())
    | ',' -> (comma ())
    | ' ' | '\n' -> FileReader.next_char (); let token = next_token () in FileReader.previous_char (); token
    | '=' | '!' | '<' | '>' -> get_comparison_or_logical_operator () ()
    | '+' | '-' | '*' | '/' | '%' -> (
            match get_arithmetic_or_assignment_operator_or_skip_comment () () with
            | Comment ->
                    let token = next_token () in
                    FileReader.previous_char ();
                    token
            | token -> token
    )
    | '0' .. '9' -> get_number () ()
    | '_' | 'A' .. 'Z' | 'a' .. 'z' -> get_identifier () ()
    | '"' -> get_string () ()
    | '\'' -> get_character () ()
    | character -> raise (UnexpectedCharacter (character, FileReader.file_position ()))
    ) in
    FileReader.next_char ();
    token
