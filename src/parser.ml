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
 * TODO: parse mathematical expressions with parentheses.
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

let identifier stream = match Stream.peek stream with
    | Some {token = Identifier identifier} ->
            Stream.junk stream;
            identifier
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
    | None -> failwith "Unreachable code."

let post_incrementation stream =
    let variable_name = identifier stream in
    eat PlusPlus stream;
    Ast.Increment variable_name

let rec argument stream = expression stream

and arguments stream = separated_by argument Comma stream

and variable_assignment variable_name stream =
    eat Equal stream;
    let variable_value = expression stream in
    Ast.Assignment {Ast.variable_name; Ast.variable_value}

and factor stream =
    match Stream.peek stream with
    | Some {token = Int integer} ->
            Stream.junk stream;
            Ast.Int integer
    | Some {token = String str} ->
            Stream.junk stream;
            Ast.String str
    | Some {token = Character str} ->
            Stream.junk stream;
            Ast.Character str
    | Some {token = Identifier identifier} ->
            Stream.junk stream;
            Ast.Variable identifier
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
    | None -> failwith "Unreachable code."

and precedence70 stream expr =
    match Stream.peek stream with
    | Some {token = Equal} ->
            (match expr with
            | Ast.Variable variable_name ->
                    variable_assignment variable_name stream
            | _ -> failwith "Expected variable before `=` token."
            )
    | _ -> expr

and precedence30 stream expr1 =
    match Stream.peek stream with
    | Some {token = Greater} ->
            Stream.junk stream;
            let expr2 = expression stream in
            Ast.Greater (expr1, expr2)
    | Some {token = GreaterOrEqual} ->
            Stream.junk stream;
            let expr2 = expression stream in
            Ast.GreaterOrEqual (expr1, expr2)
    | Some {token = IsEqual} ->
            Stream.junk stream;
            let expr2 = expression stream in
            Ast.Equals (expr1, expr2)
    | Some {token = Lesser} ->
            Stream.junk stream;
            let expr2 = expression stream in
            Ast.Lesser (expr1, expr2)
    | Some {token = LesserOrEqual} ->
            Stream.junk stream;
            let expr2 = expression stream in
            Ast.LesserOrEqual (expr1, expr2)
    | Some {token = NotEqual} ->
            Stream.junk stream;
            let expr2 = expression stream in
            Ast.NotEqual (expr1, expr2)
    | _ -> precedence70 stream expr1

and precedence20 stream expr1 =
    match Stream.peek stream with
    | Some {token = Plus} ->
            Stream.junk stream;
            let expr2 = precedence5 stream in
            precedence20 stream (Ast.Operation (Ast.Addition (expr1, expr2)))
    | Some {token = Minus} ->
            Stream.junk stream;
            let expr2 = precedence5 stream in
            precedence20 stream (Ast.Operation (Ast.Subtraction (expr1, expr2)))
    | _ -> precedence30 stream expr1

and precedence15 stream expr1 =
    match Stream.peek stream with
    | Some {token = Star} ->
            Stream.junk stream;
            let expr2 = factor stream in
            precedence15 stream (Ast.Operation (Ast.Multiplication (expr1, expr2)))
    | Some {token = Slash} ->
            Stream.junk stream;
            let expr2 = factor stream in
            precedence15 stream (Ast.Operation (Ast.Division (expr1, expr2)))
    | Some {token = Modulo} ->
            Stream.junk stream;
            let expr2 = factor stream in
            precedence15 stream (Ast.Operation (Ast.Modulo (expr1, expr2)))
    | _ -> expr1

and precedence5 stream =
    match Stream.npeek 2 stream with
    | [_; {token = LeftParenthesis}] -> function_call stream
    | [_; {token = PlusPlus}] -> post_incrementation stream
    | _ ->  let expr1 = factor stream in
            precedence15 stream expr1

and expression stream =
    let expr1 = precedence5 stream in
    precedence20 stream expr1

and function_call stream =
    let called_function_name = identifier stream in
    eat LeftParenthesis stream;
    let arguments = arguments stream in
    eat RightParenthesis stream;
    Ast.FunctionCall {Ast.called_function_name; Ast.arguments}

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
    {Ast.variable_type; Ast.variable_name; Ast.variable_value}

let variable_declaration_statement stream =
    let declaration = variable_declaration stream in
    eat SemiColon stream;
    Ast.VariableDeclaration declaration

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

and do_while_statement stream =
    eat Do stream;
    eat LeftCurlyBracket stream;
    let while_statements = statements stream in
    eat RightCurlyBracket stream;
    eat While stream;
    eat LeftParenthesis stream;
    let while_condition = expression stream in
    eat RightParenthesis stream;
    eat SemiColon stream;
    Ast.While { Ast.while_condition; Ast.while_statements }

and for_initialization stream =
    try
        match Stream.peek stream with
        | Some {token = Identifier identifier} ->
                let _ = Hashtbl.find types identifier in
                Ast.ForVariableDeclaration (variable_declaration stream)
        | _ -> failwith "Unreachable code."
    with Not_found ->
        Ast.ForExpression (expression stream)

and for_statement stream =
    eat For stream;
    eat LeftParenthesis stream;
    let for_init = for_initialization stream in
    eat SemiColon stream;
    let for_condition = expression stream in
    eat SemiColon stream;
    let for_increment = expression stream in
    eat RightParenthesis stream;
    eat LeftCurlyBracket stream;
    let for_statements = statements stream in
    eat RightCurlyBracket stream;
    Ast.For { Ast.for_init; Ast.for_condition; Ast.for_increment; Ast.for_statements }

and statement stream =
    match Stream.peek stream with
    | Some {token = Return} -> return_statement stream
    | Some {token = Identifier identifier} ->
            (try
                let _ = Hashtbl.find types identifier in
                variable_declaration_statement stream
            with Not_found ->
                expression_statement stream
            )
    | Some {token = Const} ->
            constant_declaration stream
    | Some {token = Do} ->
            do_while_statement stream
    | Some {token = For} ->
            for_statement stream
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
