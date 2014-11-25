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
 * TODO: Create helper functions like ends_with to help creating list of things.
 *)

open Lexer

type t = {
    tokens: token_with_position Stream.t;
    parser_lexer: Lexer.t;
}

exception ParseError of Lexer.error_message

let parse_error error_message error_position =
    raise (ParseError {error_message; error_position})

let list_of parsr stream =
    let rec list_of acc =
        try
            list_of (parsr stream :: acc)
        with Stream.Failure -> acc
    in List.rev (list_of [])

let optional stream parsr =
    try
        Some (parsr stream)
    with Stream.Failure -> None

let rec arguments = parser
    | [< expression = expression >] -> [expression]

and expression = parser
    | [< '{token = Character character} >] ->
            Ast.Character character
    | [< '{token = Identifier called_function_name}; f = function_call called_function_name >] ->
            f
    | [< '{token = Int integer} >] ->
            Ast.Int integer
    | [< '{token = String string_literal} >] ->
            Ast.String string_literal

and function_call called_function_name = parser
    | [< '{token = LeftParenthesis}; arguments = arguments; '{token = RightParenthesis}; '{token = SemiColon} >] ->
            Ast.FunctionCall {Ast.called_function_name; Ast.arguments}

let identifier = parser
    | [< '{token = Identifier identifier} >] ->
            identifier

let rec array_type typ = parser
    | [< '{token = LeftSquareBracket} >] ->
            array_type (Ast.Pointer typ) __strm
    | [< >] -> typ

let rec pointer_type typ = parser
    | [< '{token = Star} >] ->
        pointer_type (Ast.Pointer typ) __strm
    | [< >] -> typ

let typ = parser
    | [< '{token = Identifier typ} >] ->
            pointer_type (Ast.Type typ) __strm

let parameter = parser
    | [< parameter_type = typ; parameter_name = identifier; parameter_type = array_type parameter_type >] ->
            {
                Ast.parameter_type;
                Ast.parameter_name;
            }

let parameters = parser
    | [< parameter = parameter >] -> [parameter]
    | [< >] -> []

let equal_value = parser
    | [< '{token = Equal}; expression = expression; '{token = SemiColon} >] -> expression

let variable_declaration = parser
    | [< variable_type = typ; variable_name = identifier >] ->
            let variable_value = optional __strm equal_value in
            Ast.VariableDeclaration {Ast.variable_type; Ast.variable_name; Ast.variable_value}

let statement = parser
    (*| [< '{token = Identifier _} >] ->
            either parsr
            [ variable_declaration
            ; Ast.Expression expression
            ]*)
    | [< '{token = Return}; expression = expression; '{token = SemiColon} >] ->
            Ast.Return expression

let statements = list_of statement

let declaration = parser
    | [< return_type = typ; function_name = identifier; '{token = LeftParenthesis}; parameters = parameters; '{token = RightParenthesis}; '{token = LeftCurlyBracket}; statements = statements; '{token = RightCurlyBracket} >] ->
    Ast.FunctionDeclaration {
        Ast.return_type;
        Ast.function_name;
        Ast.parameters;
        Ast.statements;
    }

let declarations stream =
    let rec declarations acc = parser
        | [< '{token = Eof} >] -> acc
        | [< >] -> declarations (declaration stream :: acc) stream
    in List.rev (declarations [] stream)

let start parser_lexer =
    declarations (tokens parser_lexer)

let parse filename =
    let lexer = create filename in
    let ast = start lexer in
    close lexer;
    ast
