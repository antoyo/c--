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
 * TODO: Écrire des fonctions pour abstraire les concepts courants (répétitions, optionel).
 * TODO: Utiliser un GADT.
 * TODO: Écrire un point d’extension (ppx) pour rendre plus simple le parseur.
 *)

open Lexer

type t = {
    tokens: token_with_position Stream.t;
    parser_lexer: Lexer.t;
}

let types = Hashtbl.create 20

let () =
    Hashtbl.add types "char" 0;
    Hashtbl.add types "float" 0;
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

let post_decrementation stream =
    let variable_name = identifier stream in
    eat MinusMinus stream;
    Ast.Decrement variable_name

let rec arguments stream =
    match Stream.peek stream with
    | Some {token = RightParenthesis} ->
            []
    | _ ->
        let expr1 = assignment_expression stream in
        let rec arguments' expr1 =
            (match Stream.peek stream with
            | Some {token = Comma} ->
                    Stream.junk stream;
                    let expr2 = assignment_expression stream in
                    arguments' (List.append expr1 [expr2])
            | _ -> expr1
            )
        in arguments' [expr1]

and variable_assignment variable_name stream =
    eat Equal stream;
    let variable_value = assignment_expression stream in
    Ast.Assignment {Ast.variable_name; Ast.variable_value}

and assignment_add variable_name stream =
    eat PlusEqual stream;
    let variable_value = assignment_expression stream in
    Ast.AssignmentOperation (Ast.AssignAdd (variable_name, variable_value))

and assignment_minus variable_name stream =
    eat MinusEqual stream;
    let variable_value = assignment_expression stream in
    Ast.AssignmentOperation (Ast.AssignSubtract (variable_name, variable_value))

and assignment_times variable_name stream =
    eat TimesEqual stream;
    let variable_value = assignment_expression stream in
    Ast.AssignmentOperation (Ast.AssignMultiply (variable_name, variable_value))

and assignment_divide variable_name stream =
    eat DivideEqual stream;
    let variable_value = assignment_expression stream in
    Ast.AssignmentOperation (Ast.AssignDivide (variable_name, variable_value))

and assignment_modulo variable_name stream =
    eat ModuloEqual stream;
    let variable_value = assignment_expression stream in
    Ast.AssignmentOperation (Ast.AssignModulo (variable_name, variable_value))

and assignment_expression stream =
    let expr = ternary_expression stream in
    match Stream.peek stream with
    | Some {token = Equal} ->
            (match expr with
            | Ast.Variable variable_name ->
                    variable_assignment variable_name stream
            | _ -> failwith "Expected variable before `=` token."
            )
    | Some {token = PlusEqual} ->
            (match expr with
            | Ast.Variable variable_name ->
                    assignment_add variable_name stream
            | _ -> failwith "Expected variable before `+=` token."
            )
    | Some {token = MinusEqual} ->
            (match expr with
            | Ast.Variable variable_name ->
                    assignment_minus variable_name stream
            | _ -> failwith "Expected variable before `-=` token."
            )
    | Some {token = TimesEqual} ->
            (match expr with
            | Ast.Variable variable_name ->
                    assignment_times variable_name stream
            | _ -> failwith "Expected variable before `*=` token."
            )
    | Some {token = DivideEqual} ->
            (match expr with
            | Ast.Variable variable_name ->
                    assignment_divide variable_name stream
            | _ -> failwith "Expected variable before `/=` token."
            )
    | Some {token = ModuloEqual} ->
            (match expr with
            | Ast.Variable variable_name ->
                    assignment_modulo variable_name stream
            | _ -> failwith "Expected variable before `%=` token."
            )
    | _ -> expr

and ternary_expression stream = 
    let expr = logical_or_expression stream in
    match Stream.peek stream with
    | Some {token = QuestionMark} ->
            Stream.junk stream;
            let true_expression = expression stream in
            eat Colon stream;
            let false_expression = ternary_expression stream in
            Ast.Ternary { Ast.ternary_condition = expr; Ast.true_expression; Ast.false_expression }
    | _ -> expr

and logical_or_expression stream =
    let expr1 = logical_and_expression stream in
    let rec logical_or_expressions expr1 =
        match Stream.peek stream with
        | Some { token = Pipes } ->
                Stream.junk stream;
                let expr2 = logical_and_expression stream in
                logical_or_expressions (Ast.LogicalOr (expr1, expr2))
        | _ -> expr1
    in logical_or_expressions expr1

and logical_and_expression stream =
    let expr1 = relational_expression stream in
    let rec logical_and_expressions expr1 =
        match Stream.peek stream with
        | Some { token = Ampersands } ->
                Stream.junk stream;
                let expr2 = relational_expression stream in
                logical_and_expressions (Ast.LogicalAnd (expr1, expr2))
        | _ -> expr1
    in logical_and_expressions expr1

and relational_expression stream =
    let expr1 = additive_expression stream in
    match Stream.peek stream with
    | Some {token = Greater} ->
            Stream.junk stream;
            let expr2 = additive_expression stream in
            Ast.Greater (expr1, expr2)
    | Some {token = GreaterOrEqual} ->
            Stream.junk stream;
            let expr2 = additive_expression stream in
            Ast.GreaterOrEqual (expr1, expr2)
    | Some {token = IsEqual} ->
            Stream.junk stream;
            let expr2 = additive_expression stream in
            Ast.Equals (expr1, expr2)
    | Some {token = Lesser} ->
            Stream.junk stream;
            let expr2 = additive_expression stream in
            Ast.Lesser (expr1, expr2)
    | Some {token = LesserOrEqual} ->
            Stream.junk stream;
            let expr2 = additive_expression stream in
            Ast.LesserOrEqual (expr1, expr2)
    | Some {token = NotEqual} ->
            Stream.junk stream;
            let expr2 = additive_expression stream in
            Ast.NotEqual (expr1, expr2)
    | _ -> expr1

and additive_expression stream =
    let expr1 = multiplicative_expression stream in
    let rec additive_expressions expr1 =
        match Stream.peek stream with
        | Some {token = Plus} ->
                Stream.junk stream;
                let expr2 = multiplicative_expression stream in
                additive_expressions (Ast.Operation (Ast.Addition (expr1, expr2)))
        | Some {token = Minus} ->
                Stream.junk stream;
                let expr2 = multiplicative_expression stream in
                additive_expressions (Ast.Operation (Ast.Subtraction (expr1, expr2)))
        | _ -> expr1
    in additive_expressions expr1

and multiplicative_expression stream =
    let expr1 = unary_expression stream in
    let rec multiplicative_expressions expr1 =
        match Stream.peek stream with
        | Some {token = Star} ->
                Stream.junk stream;
                let expr2 = unary_expression stream in
                multiplicative_expressions (Ast.Operation (Ast.Multiplication (expr1, expr2)))
        | Some {token = Slash} ->
                Stream.junk stream;
                let expr2 = unary_expression stream in
                multiplicative_expressions (Ast.Operation (Ast.Division (expr1, expr2)))
        | Some {token = Modulo} ->
                Stream.junk stream;
                let expr2 = unary_expression stream in
                multiplicative_expressions (Ast.Operation (Ast.Modulo (expr1, expr2)))
        | _ -> expr1
    in multiplicative_expressions expr1

and unary_expression stream =
    match Stream.peek stream with
    | Some {token = Minus} ->
            Stream.junk stream;
            let expr = unary_expression stream in
            Ast.Negate expr
    | Some {token = Not} ->
            Stream.junk stream;
            let expr = unary_expression stream in
            Ast.Not expr
    | _ -> postfix_expression stream

and postfix_expression stream =
    match Stream.npeek 2 stream with
    | [_; {token = LeftParenthesis}] -> function_call stream
    | [_; {token = LeftSquareBracket}] -> array_index stream
    | [_; {token = PlusPlus}] -> post_incrementation stream
    | [_; {token = MinusMinus}] -> post_decrementation stream
    | _ ->  primary_expression stream

and primary_expression stream =
    match Stream.peek stream with
    | Some {token = LeftParenthesis} ->
            Stream.junk stream;
            let expr = expression stream in
            eat RightParenthesis stream;
            expr
    | Some {token = Float floating} ->
            Stream.junk stream;
            Ast.Float floating
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

and expression stream =
    let expr1 = assignment_expression stream in
    let rec comma_expression expr1 =
        match Stream.peek stream with
        | Some {token = Comma} ->
                Stream.junk stream;
                let expr2 = assignment_expression stream in
                comma_expression (Ast.CommaExpression (expr1, expr2))
        | _ -> expr1
    in comma_expression expr1

and array_index stream =
    let indirection_name = identifier stream in
    eat LeftSquareBracket stream;
    let indirection_index = expression stream in
    eat RightSquareBracket stream;
    Ast.Indirection {Ast.indirection_name; Ast.indirection_index}

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

let rec parameters stream =
    match Stream.peek stream with
    | Some {token = RightParenthesis} ->
            []
    | _ ->
        let parameter1 = parameter stream in
        let rec parameters' parameter1 =
            (match Stream.peek stream with
            | Some {token = Comma} ->
                    Stream.junk stream;
                    let parameter2 = parameter stream in
                    parameters' (List.append parameter1 [parameter2])
            | _ -> parameter1
            )
        in parameters' [parameter1]

let equal_value stream =
    eat Equal stream;
    assignment_expression stream

let expression_statement stream =
    let expr = expression stream in
    eat SemiColon stream;
    Ast.Expression expr

let break_statement stream =
    eat Break stream;
    eat SemiColon stream;
    Ast.Break

let return_statement stream =
    eat Return stream;
    let expression = expression stream in
    eat SemiColon stream;
    Ast.Return expression

let variable_declaration_without_type variable_type stream =
    let variable_name = identifier stream in
    let variable_value = match Stream.peek stream with
        | Some {token = Equal} -> Some (equal_value stream)
        | _ -> None
    in
    {Ast.variable_type; Ast.variable_name; Ast.variable_value}

let variable_declaration stream =
    let variable_type = typ stream in
    variable_declaration_without_type variable_type stream

let variable_declarations stream =
    let declaration1 = variable_declaration stream in
    let {Ast.variable_type} = declaration1 in
    let rec variable_declarations' declaration1 =
        match Stream.peek stream with
        | Some {token = Comma} ->
                Stream.junk stream;
                let declaration2 = variable_declaration_without_type variable_type stream in
                variable_declarations' (declaration2 :: declaration1)
        | _ -> List.rev declaration1
    in variable_declarations' [declaration1]

let variable_declaration_statement stream =
    let declarations = variable_declarations stream in
    eat SemiColon stream;
    Ast.VariableDeclarations declarations

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
    | Some {token = Case} -> []
    | Some {token = Default} -> []
    | _ ->
            let statement = statement stream in
            statement :: statements stream

and else_if_statements stream =
    match Stream.npeek 2 stream with
    | [{token = Else}; {token = If}] ->
            Stream.junk stream;
            let condition_statements = if_condition_statements stream in
            let next_if_statements = else_if_statements stream in
            condition_statements :: next_if_statements
    | _ -> []

and if_expression stream =
    eat If stream;
    eat LeftParenthesis stream;
    let if_condition = expression stream in
    eat RightParenthesis stream;
    if_condition

and bracketed_statements stream =
    eat LeftCurlyBracket stream;
    let statements_bracketed = statements stream in
    eat RightCurlyBracket stream;
    statements_bracketed

and if_condition_statements stream =
    let if_condition = if_expression stream in
    let if_statements = bracketed_statements stream in
    { Ast.if_condition; Ast.if_statements }

and if_statement stream =
    let condition_statements = if_condition_statements stream in
    let else_ifs = else_if_statements stream in
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
    Ast.If { Ast.else_ifs; Ast.else_statements; Ast.condition_statements }

and case_list stream =
    match Stream.peek stream with
    | Some {token = RightCurlyBracket} -> []
    | Some {token = Case} ->
            Stream.junk stream;
            let case_condition = expression stream in
            eat Colon stream;
            let case_instructions = statements stream in
            let next_cases = case_list stream in
            Ast.Case {Ast.case_condition; Ast.case_instructions} :: next_cases
    | Some {token = Default} ->
            Stream.junk stream;
            eat Colon stream;
            let statements = statements stream in
            let next_cases = case_list stream in
            Ast.Default statements :: next_cases
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
    | None -> failwith "Unreachable code."

and switch_statement stream =
    eat Switch stream;
    eat LeftParenthesis stream;
    let switch_expression = expression stream in
    eat RightParenthesis stream;
    eat LeftCurlyBracket stream;
    let switch_conditions = case_list stream in
    eat RightCurlyBracket stream;
    Ast.Switch { Ast.switch_expression; Ast.switch_conditions }

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
                Ast.ForVariableDeclarations (variable_declarations stream)
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
    | Some {token = Break} -> break_statement stream
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
    | Some {token = Switch} ->
            switch_statement stream
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
    (match Stream.peek stream with
    | Some {token = SemiColon} ->
            eat SemiColon stream;
            Ast.FunctionPrototype {
                Ast.return_type;
                Ast.function_name;
                Ast.parameters;
                Ast.statements = [];
            }
    | Some {token = LeftCurlyBracket} ->
            eat LeftCurlyBracket stream;
            let statements = statements stream in
            eat RightCurlyBracket stream;
            Ast.FunctionDeclaration {
                Ast.return_type;
                Ast.function_name;
                Ast.parameters;
                Ast.statements;
            }
    | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
    | None -> failwith "Unreachable code."
    )

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
