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

let eat token_to_eat stream = match Stream.peek stream with
    | Some {token} when token = token_to_eat -> Stream.junk stream

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

let rec arguments stream =
    let expression = expression stream in
    [expression]

and expression stream =
    let expr = (match Stream.peek stream with
    | Some {token = Character character} ->
            Ast.Character character
    | Some {token = Identifier called_function_name} ->
            function_call called_function_name stream
    | Some {token = Int integer} ->
            Ast.Int integer
    | Some {token = String string_literal} ->
            Ast.String string_literal
    ) in
    Stream.junk stream;
    expr

and function_call called_function_name stream =
    eat LeftParenthesis stream;
    let arguments = arguments stream in
    eat RightParenthesis stream;
    eat SemiColon stream;
    Ast.FunctionCall {Ast.called_function_name; Ast.arguments}

let identifier stream = match Stream.peek stream with
    | Some {token = Identifier identifier} ->
            Stream.junk stream;
            identifier

let rec array_type typ stream = match Stream.peek stream with
    | Some {token = LeftSquareBracket} ->
            array_type (Ast.Pointer typ) stream
    | _ -> typ

let rec pointer_type typ stream = match Stream.peek stream with
    | Some {token = Star} ->
        Stream.junk stream;
        pointer_type (Ast.Pointer typ) stream
    | _ -> typ

let typ stream =
    let typ = identifier stream in
    pointer_type (Ast.Type typ) stream

let parameter stream =
    let parameter_type = typ stream in
    let parameter_name = identifier stream in
    let parameter_type = array_type parameter_type stream in
    {
        Ast.parameter_type;
        Ast.parameter_name;
    }

let parameters stream = match Stream.peek stream with
    | Some {token = RightParenthesis} -> []
    | _ -> let parameter = parameter stream in
           [parameter]

let equal_value stream =
    eat Equal stream;
    let expression = expression stream in
    eat SemiColon stream;
    expression

let variable_declaration stream =
    let variable_type = typ stream in
    let variable_name = identifier stream in
    let variable_value = optional stream equal_value in
    Ast.VariableDeclaration {Ast.variable_type; Ast.variable_name; Ast.variable_value}

let statement stream =
    eat Return stream;
    let expression = expression stream in
    eat SemiColon stream;
    Ast.Return expression
    (*| [< '{token = Identifier _} >] ->
            either parsr
            [ variable_declaration
            ; Ast.Expression expression
            ]*)

let statements stream = [statement stream]

let declaration stream =
    let return_type = typ stream in
    let function_name = identifier stream in
    eat LeftParenthesis stream;
    let parameters = parameters stream in
    eat RightParenthesis stream;
    eat LeftCurlyBracket stream;
    let statements = statements stream in
    eat RightCurlyBracket stream;
    Ast.FunctionDeclaration {
        Ast.return_type;
        Ast.function_name;
        Ast.parameters;
        Ast.statements;
    }

let rec declarations stream =
    match Stream.peek stream with
    | Some {token = Eof} | None -> []
    | Some token ->
        let declaration = declaration stream in
        declaration :: declarations stream

let rec print_tokens stream = match Stream.peek stream with
    | Some token -> trace token; Stream.junk stream; print_tokens stream
    | None -> ()

let start parser_lexer =
    let tokens = tokens parser_lexer in
    declarations tokens

let parse filename =
    let lexer = create filename in
    let ast = start lexer in
    close lexer;
    ast
