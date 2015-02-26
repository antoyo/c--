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
 * TODO: Create helper functions like list_of, ends_with to help creating list of things.
 *)

open Lexer

type t = {
    tokens: token_with_position Stream.t;
    parser_lexer: Lexer.t;
}

let types = Hashtbl.create 20

let () =
    Hashtbl.add types "char" 0;
    Hashtbl.add types "int" 0

exception ParseError of Lexer.error_message

let parse_error error_message error_position =
    raise (ParseError {error_message; error_position})

let eat token_to_eat stream = match Stream.peek stream with
    | Some {token} when token = token_to_eat -> Stream.junk stream
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position
    | None -> failwith "Unreachable code."

let rec either = function
    | [] -> failwith "Unreachable code."
    | expr :: rest ->
            try
                Lazy.force expr
            with ParseError _ ->
                either rest

let optional parsr stream =
    try
        Some (parsr stream)
    with ParseError _ -> None

let separated_by parsr separator stream =
    let rec separated_by elements =
        try
            let element = parsr stream in
            match Stream.peek stream with
            | Some {token} when token = separator ->
                    Stream.junk stream;
                    separated_by (element :: elements)
            | _ -> element :: elements
        with ParseError _ ->
            elements
    in List.rev (separated_by [])

let variable_expression variable_name =
    Ast.Variable variable_name

let post_incrementation variable_name stream =
    eat PlusPlus stream;
    Ast.Increment variable_name

let rec argument stream = expression stream

and arguments stream = separated_by argument Comma stream

and variable_assignment variable_name stream =
    eat Equal stream;
    let variable_value = expression stream in
    Ast.Assignment {Ast.variable_name; Ast.variable_value}

and comparison_variable_expression identifier stream =
    match Stream.peek stream with
    | Some {token = Greater} ->
            Stream.junk stream;
            let expr = expression stream in
            Ast.Greater (variable_expression identifier, expr)
    | Some {token = GreaterOrEqual} ->
            Stream.junk stream;
            let expr = expression stream in
            Ast.GreaterOrEqual (variable_expression identifier, expr)
    | Some {token = IsEqual} ->
            Stream.junk stream;
            let expr = expression stream in
            Ast.Equals (variable_expression identifier, expr)
    | Some {token = Lesser} ->
            Stream.junk stream;
            let expr = expression stream in
            Ast.Lesser (variable_expression identifier, expr)
    | Some {token = LesserOrEqual} ->
            Stream.junk stream;
            let expr = expression stream in
            Ast.LesserOrEqual (variable_expression identifier, expr)
    | Some {token = NotEqual} ->
            Stream.junk stream;
            let expr = expression stream in
            Ast.NotEqual (variable_expression identifier, expr)
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
    | None -> failwith "Unreachable code."

and expression stream =
    let expr = (match Stream.peek stream with
    | Some {token = Character character} ->
            Stream.junk stream;
            Ast.Character character
    | Some {token = Identifier identifier} ->
            Stream.junk stream;
            either
            [ lazy (function_call identifier stream)
            ; lazy (variable_assignment identifier stream)
            ; lazy (post_incrementation identifier stream)
            ; lazy (comparison_variable_expression identifier stream)
            ; lazy (variable_expression identifier)
            ]
    | Some {token = Int integer} ->
            Stream.junk stream;
            Ast.Int integer
    | Some {token = String string_literal} ->
            Stream.junk stream;
            Ast.String string_literal
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
    | None -> failwith "Unreachable code."
    ) in
    expr

and function_call called_function_name stream =
    eat LeftParenthesis stream;
    let arguments = arguments stream in
    eat RightParenthesis stream;
    Ast.FunctionCall {Ast.called_function_name; Ast.arguments}

let identifier stream = match Stream.peek stream with
    | Some {token = Identifier identifier} ->
            Stream.junk stream;
            identifier
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
    | None -> failwith "Unreachable code."

let rec array_type typ stream = match Stream.peek stream with
    | Some {token = LeftSquareBracket} ->
            eat LeftSquareBracket stream;
            eat RightSquareBracket stream;
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

let rec parameters stream = separated_by parameter Comma stream

let equal_value stream =
    eat Equal stream;
    let expression = expression stream in
    expression

let expression_statement stream =
    let expr = expression stream in
    eat SemiColon stream;
    Ast.Expression expr

let return_statement stream =
    eat Return stream;
    let expression = expression stream in
    eat SemiColon stream;
    Ast.Return expression

let variable_declaration stream =
    let variable_type = typ stream in
    let variable_name = identifier stream in
    let variable_value = optional equal_value stream in
    eat SemiColon stream;
    Ast.VariableDeclaration {Ast.variable_type; Ast.variable_name; Ast.variable_value}

let constant_declaration stream =
    eat Const stream;
    let constant_type = typ stream in
    let constant_name = identifier stream in
    let constant_value = equal_value stream in
    eat SemiColon stream;
    Ast.ConstantDeclaration {Ast.constant_type; Ast.constant_name; Ast.constant_value}

let rec statements stream =
    match Stream.peek stream with
    | Some {token = RightCurlyBracket} -> []
    | _ ->
            let statement = statement stream in
            statement :: statements stream

and if_statement stream =
    eat If stream;
    eat LeftParenthesis stream;
    let if_condition = expression stream in
    eat RightParenthesis stream;
    eat LeftCurlyBracket stream;
    let if_statements = statements stream in
    eat RightCurlyBracket stream;
    let else_statements =
        match Stream.peek stream with
        | Some {token = Else} ->
                Stream.junk stream;
                eat LeftCurlyBracket stream;
                let statements = statements stream in
                eat RightCurlyBracket stream;
                Some statements
        | _ -> None
    in
    Ast.If { Ast.else_statements; Ast.if_condition; Ast.if_statements }

and while_statement stream =
    eat While stream;
    eat LeftParenthesis stream;
    let while_condition = expression stream in
    eat RightParenthesis stream;
    eat LeftCurlyBracket stream;
    let while_statements = statements stream in
    eat RightCurlyBracket stream;
    Ast.While { Ast.while_condition; Ast.while_statements }

and statement stream =
    match Stream.peek stream with
    | Some {token = Return} -> return_statement stream
    | Some {token = Identifier identifier} ->
            (try
                let _ = Hashtbl.find types identifier in
                variable_declaration stream
            with Not_found ->
                expression_statement stream
            )
    | Some {token = Const} ->
            constant_declaration stream
    | Some {token = If} ->
            if_statement stream
    | Some {token = While} ->
            while_statement stream
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
    | None -> failwith "Unreachable code."

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
