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

let eof = char_of_int 4

type file_position = int * int

exception UnexpectedCharacter of char * file_position

type token =
    | Character of char
    | Colon
    | Comma
    | Comment
    | Divide
    | DivideEqual
    | Eof
    | Equal
    | Float of float
    | Greater
    | GreaterOrEqual
    | Identifier of string
    | Int of int
    | IsEqual
    | LeftCurlyBracket
    | LeftParenthesis
    | LeftSquareBracket
    | Lesser
    | LesserOrEqual
    | Minus
    | MinusEqual
    | Modulo
    | ModuloEqual
    | Not
    | NotEqual
    | Plus
    | PlusEqual
    | RightCurlyBracket
    | RightParenthesis
    | RightSquareBracket
    | SemiColon
    | String of string
    | Times
    | TimesEqual

type token_with_position = token * file_position

let keywords = []

let add_position token = (token, FileReader.file_position ())

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
    | '+' -> if_match_after '=' PlusEqual Plus
    | '-' -> if_match_after '=' MinusEqual Minus
    | '*' -> if_match_after '=' TimesEqual Times
    | '/' ->
            FileReader.next_char ();
            (match FileReader.get_char () with
            | '/' -> skip_line_comment (); Comment
            | '*' -> skip_block_comment (); Comment
            | '=' -> DivideEqual
            | _ ->
                FileReader.previous_char ();
                Divide
            )
    | '%' -> if_match_after '=' ModuloEqual Modulo
    | _ -> raise (Invalid_argument "Invalid character")

let get_comparison_or_logical_operator () =
    match FileReader.get_char () with
    | '=' -> if_match_after '=' IsEqual Equal
    | '<' -> if_match_after '=' LesserOrEqual Lesser
    | '>' -> if_match_after '=' GreaterOrEqual Greater
    | '!' -> if_match_after '=' NotEqual Not
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
                Float (float_of_string (FileReader.substring ()))
        | 'e' | 'E' ->
                FileReader.next_char ();
                get_decimals ();
                Float (float_of_string (FileReader.substring ()))
        | _ ->
                FileReader.previous_char ();
                Int (int_of_string (FileReader.substring ()))
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
                Identifier (FileReader.substring ())
    in get_identifier ()

let get_string () =
    FileReader.next_char ();
    FileReader.adjust_start_position ();
    let rec get_string () =
        match FileReader.get_char () with
        | '"' ->
                FileReader.previous_char ();
                let token = String (FileReader.substring ()) in
                FileReader.next_char ();
                token
        | _ ->
                FileReader.next_char ();
                get_string ()
    in get_string ()

let get_character () =
    FileReader.next_char ();
    let token = Character (FileReader.get_char ()) in
    FileReader.next_char ();
    token

let lex filename =
    FileReader.open_file filename

let rec next_token () : token_with_position =
    FileReader.adjust_start_position ();
    let token = (match FileReader.get_char () with
    | exception End_of_file -> Eof
    | '{' -> LeftCurlyBracket
    | '}' -> RightCurlyBracket
    | '(' -> LeftParenthesis
    | ')' -> RightParenthesis
    | '[' -> LeftSquareBracket
    | ']' -> RightSquareBracket
    | ':' -> Colon
    | ';' -> SemiColon
    | ',' -> Comma
    | ' ' | '\n' ->
            FileReader.next_char ();
            let (token, _) = next_token ()
            in FileReader.previous_char ();
            token
    | '=' | '!' | '<' | '>' -> get_comparison_or_logical_operator ()
    | '+' | '-' | '*' | '/' | '%' -> (
            match get_arithmetic_or_assignment_operator_or_skip_comment () with
            | Comment ->
                    let (token, _) = next_token () in
                    FileReader.previous_char ();
                    token
            | token -> token
    )
    | '0' .. '9' -> get_number ()
    | '_' | 'A' .. 'Z' | 'a' .. 'z' -> get_identifier ()
    | '"' -> get_string ()
    | '\'' -> get_character ()
    | character -> raise (UnexpectedCharacter (character, FileReader.file_position ()))
    ) in
    FileReader.next_char ();
    add_position token
