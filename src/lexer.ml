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
 *)

type error_message = {
    error_message: string;
    error_position: FileReader.file_position;
}

exception SyntaxError of error_message
exception UnexpectedCharacter of error_message

type token =
    | Ampersand
    | Ampersands
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
    | Pipe
    | Pipes
    | Plus
    | PlusEqual
    | PlusPlus
    | QuestionMark
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

let eof = char_of_int 4

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

let keywords = Hashtbl.create (List.length keyword_list)

let () = List.iter (fun (key, value) -> Hashtbl.add keywords key value) keyword_list

let string_of_token = function
    | { token = Ampersand } -> "&"
    | { token = Ampersands } -> "&&"
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
    | { token = Pipe } -> "|"
    | { token = Pipes } -> "||"
    | { token = Plus } -> "Plus"
    | { token = PlusEqual } -> "PlusEqual"
    | { token = PlusPlus } -> "PlusPlus"
    | { token = QuestionMark } -> "QuestionMark"
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

let trace token = print_endline (string_of_token token)

let trace_stream stream = match Stream.peek stream with
    | Some token -> trace token
    | None -> ()

class lexer filename =
    object (self)
        val reader = FileReader.open_file filename

        method private add_position token = {
            token;
            token_position = FileReader.file_position reader;
        }

        method close =
            FileReader.close_file reader

        method private escape_char = function
            | 'b' -> '\b'
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | '\\' -> '\\'
            | '\'' -> '\''
            | character -> raise_unexpected_character reader character

        method private escape_char_string = function
            | '"' -> '"'
            | character -> self#escape_char character

        method private get =
            FileReader.get_char reader

        method private get_and_or_logical_and =
            self#if_match_after '&' Ampersands Ampersand

        method private get_arithmetic_or_assignment_operator_or_skip_comment =
            match self#get with
            | '+' -> Some (self#if_match_after '=' PlusEqual (self#if_match_after '+' PlusPlus Plus))
            | '-' -> Some (self#if_match_after '=' MinusEqual (self#if_match_after '-' MinusMinus Minus))
            | '*' -> Some (self#if_match_after '=' TimesEqual Star)
            | '/' ->
                    (match self#get_next with
                    | '/' -> self#skip_line_comment; None
                    | '*' -> self#next; self#skip_block_comment; None
                    | '=' -> self#next; Some DivideEqual
                    | _ -> Some Slash
                    )
            | '%' -> Some (self#if_match_after '=' ModuloEqual Modulo)
            | _ -> raise (Invalid_argument "Invalid character")

        method private get_character =
            self#next;
            let token = match self#get with
            | '\\' ->
                    self#next;
                    Character (self#escape_char self#get)
            | character -> Character character
            in
            self#next;
            token

        method private get_comparison_or_logical_operator =
            match self#get with
            | '=' -> self#if_match_after '=' IsEqual Equal
            | '<' -> self#if_match_after '=' LesserOrEqual Lesser
            | '>' -> self#if_match_after '=' GreaterOrEqual Greater
            | '!' -> self#if_match_after '=' NotEqual Not
            | _ -> raise (Invalid_argument "Invalid character")

        method private get_decimals buffer =
            let rec get_decimals () =
                match self#get_next with
                | '0' .. '9' as character ->
                        Buffer.add_char buffer character;
                        self#next;
                        get_decimals ()
                | _ -> ()
            in get_decimals ()

        method private get_exponent buffer =
            match self#get_next with
            | 'e' | 'E' as character ->
                    Buffer.add_char buffer character;
                    self#next;
                    self#get_decimals buffer
            | _ -> ()

        method private get_identifier =
            let buffer = Buffer.create 10 in
            Buffer.add_char buffer self#get;
            let rec get_identifier () =
                match self#get_next with
                | '_' | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9'as character ->
                        Buffer.add_char buffer character;
                        self#next;
                        get_identifier ()
                | _ ->
                        self#get_identifier_or_token (Buffer.contents buffer)
            in get_identifier ()

        method private get_identifier_or_token str =
            match Hashtbl.find keywords str with
            | token -> token
            | exception Not_found -> Identifier str

        method private get_next =
            FileReader.get_next_char reader

        method private get_number =
            let buffer = Buffer.create 10 in
            Buffer.add_char buffer self#get;
            let rec get_number () =
                match self#get_next with
                | '0' .. '9' as character ->
                        Buffer.add_char buffer character;
                        self#next;
                        get_number ()
                | '.' ->
                        Buffer.add_char buffer '.';
                        self#next;
                        self#get_decimals buffer;
                        self#get_exponent buffer;
                        Float (float_of_string (Buffer.contents buffer))
                | 'e' | 'E' ->
                        Buffer.add_char buffer 'e';
                        self#next;
                        self#get_decimals buffer;
                        Float (float_of_string (Buffer.contents buffer))
                | _ ->
                        Int (int_of_string (Buffer.contents buffer))
            in get_number ()

        method private get_or_or_logical_or =
            self#if_match_after '|' Pipes Pipe

        method private get_string =
            let rec get_string buffer =
                match self#get_next with
                | '\\' ->
                        self#next;
                        self#next;
                        if self#get <> '\n'
                            then Buffer.add_char buffer (self#escape_char_string self#get);
                        get_string buffer
                | '"' ->
                        let token = String (Buffer.contents buffer) in
                        self#next;
                        token
                | '\n' | '\r' ->
                        raise_syntax_error reader "Unclosed string"
                | character ->
                        Buffer.add_char buffer character;
                        self#next;
                        get_string buffer
            in get_string (Buffer.create 10)

        method private if_match_after character result_if_true result_if_false =
            if self#get_next = character then (
                self#next;
                result_if_true;
            )
            else (
                result_if_false
            )

        method private next =
            FileReader.next_char reader

        method private next_token =
            match self#get with
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
            | '?' -> Some QuestionMark
            | '&' -> Some self#get_and_or_logical_and
            | '|' -> Some self#get_or_or_logical_or
            | ' ' | '\n' -> None
            | '=' | '!' | '<' | '>' -> Some self#get_comparison_or_logical_operator
            | '+' | '-' | '*' | '/' | '%' -> self#get_arithmetic_or_assignment_operator_or_skip_comment
            | '#' -> self#skip_line_comment; None
            | '0' .. '9' -> Some self#get_number
            | '_' | 'A' .. 'Z' | 'a' .. 'z' -> Some self#get_identifier
            | '"' -> Some self#get_string
            | '\'' -> Some self#get_character
            | character -> raise_unexpected_character reader character

        method private skip_block_comment =
            self#next;
            match self#get with
            | '*' ->
                    self#next;
                    (match self#get with
                    | '/' -> self#next
                    | _ -> self#skip_block_comment)
            | _ -> self#skip_block_comment
            | exception End_of_file -> raise_syntax_error reader "Unclosed comment"

        method private skip_line_comment =
            self#next;
            match self#get with
            | '\n' -> ()
            | _ -> self#skip_line_comment

        method tokens =
            let rec tokens stream =
                let token = self#next_token in
                self#next;
                (match token with
                | None ->  tokens stream
                | Some Eof -> stream
                | Some token -> tokens (Stream.iapp stream (Stream.ising (self#add_position token)))
                )
            in tokens Stream.sempty
    end
