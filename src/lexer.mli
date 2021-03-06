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

type error_message = {
    error_message: string;
    error_position: FileReader.file_position;
}

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

exception SyntaxError of error_message
exception UnexpectedCharacter of error_message

val string_of_token : token_with_position -> string

val trace : token_with_position -> unit

val trace_stream : token_with_position Stream.t -> unit

class lexer : string ->
    object
        val reader : FileReader.t

        method close : unit
        method tokens : token_with_position Stream.t
    end

