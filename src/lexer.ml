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
 * TODO: remember the start and end position of each token.
 * TODO: try to simplify this module (perhaps using character stream in FileReader and an extension point).
 * TODO: add the other operators and keywords supported in C.
 *)

type t = {
    lexer_file_reader: FileReader.t;
}

type error_message = {
    error_message: string;
    error_position: FileReader.file_position;
}

let eof = char_of_int 4

exception SyntaxError of error_message
exception UnexpectedCharacter of error_message

let raise_syntax_error reader message =
    raise (SyntaxError {
        error_message = message;
        error_position = FileReader.file_position reader;
    })

let raise_unexpected_character reader character =
    raise (UnexpectedCharacter {
        error_message = "Unexpected character `" ^ (Char.escaped character) ^ "`";
        error_position = FileReader.file_position reader;
    })

type token =
    | Break
    | Case
    | Character of char
    | Colon
    | Comma
    | Const
    | Default
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
    | Slash
    | Star
    | String of string
    | Switch
    | TimesEqual
    | While

type token_with_position = {
    token: token;
    token_position: FileReader.file_position;
}

let string_of_token = function
    | { token = Break } -> "Break"
    | { token = Case } -> "Case"
    | { token = Character c } -> "Character " ^ String.make 1 c
    | { token = Colon } -> "Colon"
    | { token = Comma } -> "Comma"
    | { token = Const } -> "Const"
    | { token = Default } -> "Default"
    | { token = DivideEqual } -> "DivideEqual"
    | { token = Do } -> "Do"
    | { token = Else } -> "Else"
    | { token = Eof } -> "Eof"
    | { token = Equal } -> "Equal"
    | { token = Float f } -> "Float " ^ string_of_float f
    | { token = For } -> "For"
    | { token = Greater } -> "Greater"
    | { token = GreaterOrEqual } -> "GreaterOrEqual"
    | { token = Identifier i } -> "Identifier " ^ i
    | { token = If } -> "If"
    | { token = Int i } -> "Int " ^ string_of_int i
    | { token = IsEqual } -> "IsEqual"
    | { token = LeftCurlyBracket } -> "LeftCurlyBracket"
    | { token = LeftParenthesis } -> "LeftParenthesis"
    | { token = LeftSquareBracket } -> "LeftSquareBracket"
    | { token = Lesser } -> "Lesser"
    | { token = LesserOrEqual } -> "LesserOrEqual"
    | { token = Minus } -> "Minus"
    | { token = MinusEqual } -> "MinusEqual"
    | { token = MinusMinus } -> "MinusMinus"
    | { token = Modulo } -> "Modulo"
    | { token = ModuloEqual } -> "ModuloEqual"
    | { token = Not } -> "Not"
    | { token = NotEqual } -> "NotEqual"
    | { token = Plus } -> "Plus"
    | { token = PlusEqual } -> "PlusEqual"
    | { token = PlusPlus } -> "PlusPlus"
    | { token = Return } -> "Return"
    | { token = RightCurlyBracket } -> "RightCurlyBracket"
    | { token = RightParenthesis } -> "RtParenthesis"
    | { token = RightSquareBracket } -> "RightSquareBracket"
    | { token = SemiColon } -> "SemiColon"
    | { token = Slash } -> "Slash"
    | { token = Star } -> "Star"
    | { token = String s } -> "String " ^ s
    | { token = Switch } -> "Switch"
    | { token = TimesEqual } -> "TimesEqual"
    | { token = While } -> "While"

let trace token = print_endline (string_of_token token)

let trace_stream stream = match Stream.peek stream with
    | Some token -> trace token
    | None -> ()

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

let add_position reader token = {
    token;
    token_position = FileReader.file_position reader;
}

let close lexer =
    let { lexer_file_reader = file_reader } = lexer in
    FileReader.close_file file_reader

let if_match_after reader character result_if_true result_if_false =
    if FileReader.get_next_char reader = character then (
        FileReader.next_char reader;
        result_if_true;
    )
    else (
        result_if_false
    )

let rec skip_block_comment reader =
    FileReader.next_char reader;
    match FileReader.get_char reader with
    | '*' ->
            FileReader.next_char reader;
            (match FileReader.get_char reader with
            | '/' -> FileReader.next_char reader
            | _ -> skip_block_comment reader)
    | _ -> skip_block_comment reader
    | exception End_of_file -> raise_syntax_error reader "Unclosed comment"

let rec skip_line_comment reader =
    FileReader.next_char reader;
    match FileReader.get_char reader with
    | '\n' -> ()
    | _ -> skip_line_comment reader

let get_arithmetic_or_assignment_operator_or_skip_comment reader =
    match FileReader.get_char reader with
    | '+' -> Some (if_match_after reader '=' PlusEqual (if_match_after reader '+' PlusPlus Plus))
    | '-' -> Some (if_match_after reader '=' MinusEqual (if_match_after reader '-' MinusMinus Minus))
    | '*' -> Some (if_match_after reader '=' TimesEqual Star)
    | '/' ->
            (match FileReader.get_next_char reader with
            | '/' -> skip_line_comment reader; None
            | '*' -> FileReader.next_char reader; skip_block_comment reader; None
            | '=' -> FileReader.next_char reader; Some DivideEqual
            | _ -> Some Slash
            )
    | '%' -> Some (if_match_after reader '=' ModuloEqual Modulo)
    | _ -> raise (Invalid_argument "Invalid character")

let get_comparison_or_logical_operator reader =
    match FileReader.get_char reader with
    | '=' -> if_match_after reader '=' IsEqual Equal
    | '<' -> if_match_after reader '=' LesserOrEqual Lesser
    | '>' -> if_match_after reader '=' GreaterOrEqual Greater
    | '!' -> if_match_after reader '=' NotEqual Not
    | _ -> raise (Invalid_argument "Invalid character")

let get_decimals reader buffer =
    let rec get_decimals () =
        match FileReader.get_next_char reader with
        | '0' .. '9' as character ->
                Buffer.add_char buffer character;
                FileReader.next_char reader;
                get_decimals ()
        | _ -> ()
    in get_decimals ()

let get_exponent reader buffer =
    match FileReader.get_next_char reader with
    | 'e' | 'E' as character ->
            Buffer.add_char buffer character;
            FileReader.next_char reader;
            get_decimals reader buffer
    | _ -> ()

let get_number reader =
    let buffer = Buffer.create 10 in
    Buffer.add_char buffer (FileReader.get_char reader);
    let rec get_number () =
        match FileReader.get_next_char reader with
        | '0' .. '9' as character ->
                Buffer.add_char buffer character;
                FileReader.next_char reader;
                get_number ()
        | '.' ->
                Buffer.add_char buffer '.';
                FileReader.next_char reader;
                get_decimals reader buffer;
                get_exponent reader buffer;
                Float (float_of_string (Buffer.contents buffer))
        | 'e' | 'E' ->
                Buffer.add_char buffer 'e';
                FileReader.next_char reader;
                get_decimals reader buffer;
                Float (float_of_string (Buffer.contents buffer))
        | _ ->
                Int (int_of_string (Buffer.contents buffer))
    in get_number ()

let get_identifier_or_token str =
    match Hashtbl.find keywords str with
    | token -> token
    | exception Not_found -> Identifier str

let get_identifier reader =
    let buffer = Buffer.create 10 in
    Buffer.add_char buffer (FileReader.get_char reader);
    let rec get_identifier () =
        match FileReader.get_next_char reader with
        | '_' | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'as character ->
                Buffer.add_char buffer character;
                FileReader.next_char reader;
                get_identifier ()
        | _ ->
                get_identifier_or_token (Buffer.contents buffer)
    in get_identifier ()

let escape_char reader = function
    | 'b' -> '\b'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | '\\' -> '\\'
    | '\'' -> '\''
    | character -> raise_unexpected_character reader character

let escape_char_string reader = function
    | '"' -> '"'
    | character -> escape_char reader character

let get_string reader =
    let rec get_string buffer =
        match FileReader.get_next_char reader with
        | '\\' ->
                FileReader.next_char reader;
                FileReader.next_char reader;
                if FileReader.get_char reader <> '\n'
                    then Buffer.add_char buffer (escape_char_string reader (FileReader.get_char reader));
                get_string buffer
        | '"' ->
                let token = String (Buffer.contents buffer) in
                FileReader.next_char reader;
                token
        | '\n' | '\r' ->
                raise_syntax_error reader "Unclosed string"
        | character ->
                Buffer.add_char buffer character;
                FileReader.next_char reader;
                get_string buffer
    in get_string (Buffer.create 10)

let get_character reader =
    FileReader.next_char reader;
    let token = match FileReader.get_char reader with
    | '\\' ->
            FileReader.next_char reader;
            Character (escape_char reader (FileReader.get_char reader))
    | character -> Character character
    in
    FileReader.next_char reader;
    token

let next_token file_reader =
    match FileReader.get_char file_reader with
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
    | '=' | '!' | '<' | '>' -> Some (get_comparison_or_logical_operator file_reader)
    | '+' | '-' | '*' | '/' | '%' -> get_arithmetic_or_assignment_operator_or_skip_comment file_reader
    | '0' .. '9' -> Some (get_number file_reader)
    | '_' | 'A' .. 'Z' | 'a' .. 'z' -> Some (get_identifier file_reader)
    | '"' -> Some (get_string file_reader)
    | '\'' -> Some (get_character file_reader)
    | character -> raise_unexpected_character file_reader character

let tokens lexer =
    let { lexer_file_reader = file_reader } = lexer in
    let rec tokens stream =
        let token = next_token file_reader in
        FileReader.next_char file_reader;
        (match token with
        | None ->  tokens stream
        | Some Eof -> stream
        | Some token -> tokens (Stream.iapp stream (Stream.ising (add_position file_reader token)))
        )
    in tokens Stream.sempty

let create filename =
    let lexer_file_reader = FileReader.open_file filename in
    { lexer_file_reader }
