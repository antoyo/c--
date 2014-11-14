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
 * TODO: detect unclosed comment (at end of file) and unclosed string.
 * TODO: try to simplify this module (perhaps using character stream in FileReader and an extension point).
 * TODO: allow multiple instances of the modules to be created.
 *)

let eof = char_of_int 4

type file_position = int * int

exception UnexpectedCharacter of char * file_position

let raise_unexpected_character character =
    raise (UnexpectedCharacter (character, FileReader.file_position ()))

type token =
    | Break
    | Case
    | Character of char
    | Colon
    | Comma
    | Const
    | Default
    | Divide
    | DivideEqual
    | Do
    | Else
    | Eof
    | Equal
    | Float of float
    | For
    | Greater
    | GreaterOrEqual
    | Identifier of string
    | If
    | Int of int
    | IsEqual
    | LeftCurlyBracket
    | LeftParenthesis
    | LeftSquareBracket
    | Lesser
    | LesserOrEqual
    | Minus
    | MinusEqual
    | MinusMinus
    | Modulo
    | ModuloEqual
    | Not
    | NotEqual
    | Plus
    | PlusEqual
    | PlusPlus
    | Return
    | RightCurlyBracket
    | RightParenthesis
    | RightSquareBracket
    | SemiColon
    | String of string
    | Switch
    | Times
    | TimesEqual
    | While

type token_with_position = token * file_position

let keyword_list =
    [ ("break", Break)
    ; ("case", Case)
    ; ("const", Const)
    ; ("default", Default)
    ; ("do", Do)
    ; ("else", Else)
    ; ("for", For)
    ; ("if", If)
    ; ("return", Return)
    ; ("switch", Switch)
    ; ("while", While)
    ]

let keywords = Hashtbl.create 10
let () = List.iter (fun (key, value) -> Hashtbl.add keywords key value) keyword_list

let add_position token = (token, FileReader.file_position ())

let close () =
    FileReader.close_file ()

let if_match_after character result_if_true result_if_false =
    if FileReader.get_next_char () = character then (
        FileReader.next_char ();
        result_if_true;
    )
    else (
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
    | '+' -> Some (if_match_after '=' PlusEqual (if_match_after '+' PlusPlus Plus))
    | '-' -> Some (if_match_after '=' MinusEqual (if_match_after '-' MinusMinus Minus))
    | '*' -> Some (if_match_after '=' TimesEqual Times)
    | '/' ->
            (match FileReader.get_next_char () with
            | '/' -> skip_line_comment (); None
            | '*' -> FileReader.next_char (); skip_block_comment (); None
            | '=' -> FileReader.next_char (); Some DivideEqual
            | _ -> Some Divide
            )
    | '%' -> Some (if_match_after '=' ModuloEqual Modulo)
    | _ -> raise (Invalid_argument "Invalid character")

let get_comparison_or_logical_operator () =
    match FileReader.get_char () with
    | '=' -> if_match_after '=' IsEqual Equal
    | '<' -> if_match_after '=' LesserOrEqual Lesser
    | '>' -> if_match_after '=' GreaterOrEqual Greater
    | '!' -> if_match_after '=' NotEqual Not
    | _ -> raise (Invalid_argument "Invalid character")

let get_decimals buffer =
    let rec get_decimals () =
        match FileReader.get_next_char () with
        | '0' .. '9' as character ->
                Buffer.add_char buffer character;
                FileReader.next_char ();
                get_decimals ()
        | _ -> ()
    in get_decimals ()

let get_exponent buffer =
    match FileReader.get_next_char () with
    | 'e' | 'E' as character ->
            Buffer.add_char buffer character;
            FileReader.next_char ();
            get_decimals buffer
    | _ -> ()

let get_number () =
    let buffer = Buffer.create 10 in
    Buffer.add_char buffer (FileReader.get_char ());
    let rec get_number () =
        match FileReader.get_next_char () with
        | '0' .. '9' as character ->
                Buffer.add_char buffer character;
                FileReader.next_char ();
                get_number ()
        | '.' ->
                Buffer.add_char buffer '.';
                FileReader.next_char ();
                get_decimals buffer;
                get_exponent buffer;
                Float (float_of_string (Buffer.contents buffer))
        | 'e' | 'E' ->
                Buffer.add_char buffer 'e';
                FileReader.next_char ();
                get_decimals buffer;
                Float (float_of_string (Buffer.contents buffer))
        | _ ->
                Int (int_of_string (Buffer.contents buffer))
    in get_number ()

let get_identifier_or_token str =
    match Hashtbl.find keywords str with
    | token -> token
    | exception Not_found -> Identifier str

let get_identifier () =
    let buffer = Buffer.create 10 in
    Buffer.add_char buffer (FileReader.get_char ());
    let rec get_identifier () =
        match FileReader.get_next_char () with
        | '_' | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'as character ->
                Buffer.add_char buffer character;
                FileReader.next_char ();
                get_identifier ()
        | _ ->
                get_identifier_or_token (Buffer.contents buffer)
    in get_identifier ()

let escape_char = function
    | 'b' -> '\b'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | '\\' -> '\\'
    | '\'' -> '\''
    | character -> raise_unexpected_character character

let escape_char_string = function
    | '"' -> '"'
    | character -> escape_char character

let get_string () =
    let rec get_string buffer =
        match FileReader.get_next_char () with
        | '\\' ->
                FileReader.next_char ();
                FileReader.next_char ();
                Buffer.add_char buffer (escape_char_string (FileReader.get_char ()));
                get_string buffer
        | '"' ->
                let token = String (Buffer.contents buffer) in
                FileReader.next_char ();
                FileReader.next_char ();
                token
        | character ->
                Buffer.add_char buffer character;
                FileReader.next_char ();
                get_string buffer
    in get_string (Buffer.create 10)

let get_character () =
    FileReader.next_char ();
    let token = match FileReader.get_char () with
    | '\\' ->
            FileReader.next_char ();
            Character (escape_char (FileReader.get_char ()))
    | character -> Character character
    in
    FileReader.next_char ();
    token

let next_token () =
    match FileReader.get_char () with
    | exception End_of_file -> Some Eof
    | '{' -> Some LeftCurlyBracket
    | '}' -> Some RightCurlyBracket
    | '(' -> Some LeftParenthesis
    | ')' -> Some RightParenthesis
    | '[' -> Some LeftSquareBracket
    | ']' -> Some RightSquareBracket
    | ':' -> Some Colon
    | ';' -> Some SemiColon
    | ',' -> Some Comma
    | ' ' | '\n' -> None
    | '=' | '!' | '<' | '>' -> Some (get_comparison_or_logical_operator ())
    | '+' | '-' | '*' | '/' | '%' -> get_arithmetic_or_assignment_operator_or_skip_comment ()
    | '0' .. '9' -> Some (get_number ())
    | '_' | 'A' .. 'Z' | 'a' .. 'z' -> Some (get_identifier ())
    | '"' -> Some (get_string ())
    | '\'' -> Some (get_character ())
    | character -> raise_unexpected_character character

let rec stream_generator _ =
    let token = next_token () in
    FileReader.next_char();
    match token with
    | None ->  stream_generator 0
    | Some Eof -> None
    | Some token -> Some (add_position token)

let tokens filename =
    FileReader.open_file filename;
    Stream.from stream_generator
