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
 * TODO: Utiliser un GADT (faire un analyseur sémantique qui transforme un AST non typé en un AST typé).
 * TODO: Écrire un point d’extension (ppx) pour rendre plus simple le parseur.
 *)

open Lexer

exception ParseError of Lexer.error_message

let parse_error error_message error_position =
    raise (ParseError {error_message; error_position})

class parsr lexer =
    object (self)
        val stream = lexer#tokens

        val types = Hashtbl.create 20

        initializer
            Hashtbl.add types "char" 0;
            Hashtbl.add types "float" 0;
            Hashtbl.add types "int" 0

        method private additive_expression =
            let expr1 = self#multiplicative_expression in
            let rec additive_expressions expr1 =
                match Stream.peek stream with
                | Some {token = Plus} ->
                        self#drop;
                        let expr2 = self#multiplicative_expression in
                        additive_expressions (Ast.Operation (Ast.Addition (expr1, expr2)))
                | Some {token = Minus} ->
                        self#drop;
                        let expr2 = self#multiplicative_expression in
                        additive_expressions (Ast.Operation (Ast.Subtraction (expr1, expr2)))
                | _ -> expr1
            in additive_expressions expr1

        method private arguments =
            match Stream.peek stream with
            | Some {token = RightParenthesis} ->
                    []
            | _ ->
                let expr1 = self#assignment_expression in
                let rec arguments' expr1 =
                    (match Stream.peek stream with
                    | Some {token = Comma} ->
                            self#drop;
                            let expr2 = self#assignment_expression in
                            arguments' (List.append expr1 [expr2])
                    | _ -> expr1
                    )
                in arguments' [expr1]

        method private array_index =
            let indirection_name = self#identifier in
            self#eat LeftSquareBracket;
            let indirection_index = self#expression in
            self#eat RightSquareBracket;
            Ast.Indirection {Ast.indirection_name; Ast.indirection_index}

        method private array_type typ =
            match Stream.peek stream with
            | Some {token = LeftSquareBracket} ->
                    self#eat LeftSquareBracket;
                    self#eat RightSquareBracket;
                    self#array_type (Ast.Pointer typ)
            | _ -> typ

        method private assignment_add variable_name =
            self#eat PlusEqual;
            let variable_value = self#assignment_expression in
            Ast.AssignmentOperation (Ast.AssignAdd (variable_name, variable_value))

        method private assignment_divide variable_name =
            self#eat DivideEqual;
            let variable_value = self#assignment_expression in
            Ast.AssignmentOperation (Ast.AssignDivide (variable_name, variable_value))

        method private assignment_expression =
            let expr = self#ternary_expression in
            match Stream.peek stream with
            | Some {token = Equal} ->
                    (match expr with
                    | Ast.Variable variable_name ->
                            self#variable_assignment variable_name
                    | _ -> failwith "Expected variable before `=` token."
                    )
            | Some {token = PlusEqual} ->
                    (match expr with
                    | Ast.Variable variable_name ->
                            self#assignment_add variable_name
                    | _ -> failwith "Expected variable before `+=` token."
                    )
            | Some {token = MinusEqual} ->
                    (match expr with
                    | Ast.Variable variable_name ->
                            self#assignment_minus variable_name
                    | _ -> failwith "Expected variable before `-=` token."
                    )
            | Some {token = TimesEqual} ->
                    (match expr with
                    | Ast.Variable variable_name ->
                            self#assignment_times variable_name
                    | _ -> failwith "Expected variable before `*=` token."
                    )
            | Some {token = DivideEqual} ->
                    (match expr with
                    | Ast.Variable variable_name ->
                            self#assignment_divide variable_name
                    | _ -> failwith "Expected variable before `/=` token."
                    )
            | Some {token = ModuloEqual} ->
                    (match expr with
                    | Ast.Variable variable_name ->
                            self#assignment_modulo variable_name
                    | _ -> failwith "Expected variable before `%=` token."
                    )
            | _ -> expr

        method private assignment_minus variable_name =
            self#eat MinusEqual;
            let variable_value = self#assignment_expression in
            Ast.AssignmentOperation (Ast.AssignSubtract (variable_name, variable_value))

        method private assignment_modulo variable_name =
            self#eat ModuloEqual;
            let variable_value = self#assignment_expression in
            Ast.AssignmentOperation (Ast.AssignModulo (variable_name, variable_value))

        method private assignment_times variable_name =
            self#eat TimesEqual;
            let variable_value = self#assignment_expression in
            Ast.AssignmentOperation (Ast.AssignMultiply (variable_name, variable_value))

        method private bracketed_statements =
            self#eat LeftCurlyBracket;
            let statements_bracketed = self#statements in
            self#eat RightCurlyBracket;
            statements_bracketed

        method private break_statement =
            self#eat Break;
            self#eat SemiColon;
            Ast.Break

        method private case_list =
            match Stream.peek stream with
            | Some {token = RightCurlyBracket} -> []
            | Some {token = Case} ->
                    self#drop;
                    let case_condition = self#expression in
                    self#eat Colon;
                    let case_instructions = self#statements in
                    let next_cases = self#case_list in
                    Ast.Case {Ast.case_condition; Ast.case_instructions} :: next_cases
            | Some {token = Default} ->
                    self#drop;
                    self#eat Colon;
                    let statements = self#statements in
                    let next_cases = self#case_list in
                    Ast.Default statements :: next_cases
            | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
            | None -> failwith "Unreachable code."

        method private constant_declaration constant_type =
            let constant_name = self#identifier in
            let constant_value = self#equal_value in
            {Ast.constant_type; Ast.constant_name; Ast.constant_value}

        method private constant_declaration_statement =
            self#eat Const;
            let constant_type = self#typ in
            let declarations = self#constant_declarations constant_type in
            self#eat SemiColon;
            Ast.ConstantDeclarations declarations

        method private constant_declarations constant_type =
            let declaration1 = self#constant_declaration constant_type in
            let rec constant_declarations' declaration1 =
                match Stream.peek stream with
                | Some {token = Comma} ->
                        self#drop;
                        let declaration2 = self#constant_declaration constant_type in
                        constant_declarations' (declaration2 :: declaration1)
                | _ -> List.rev declaration1
            in constant_declarations' [declaration1]

        method private current_position =
            match Stream.peek stream with
            | Some {token_position} -> token_position
            | None -> failwith "Unreachable code."

        method private declaration =
            let return_type = self#typ in
            let function_name = self#identifier in
            self#eat LeftParenthesis;
            let parameters = self#parameters in
            self#eat RightParenthesis;
            (match Stream.peek stream with
            | Some {token = SemiColon} ->
                    self#eat SemiColon;
                    Ast.FunctionPrototype {
                        Ast.return_type;
                        Ast.function_name;
                        Ast.parameters;
                        Ast.statements = [];
                    }
            | Some {token = LeftCurlyBracket} ->
                    self#eat LeftCurlyBracket;
                    let statements = self#statements in
                    self#eat RightCurlyBracket;
                    Ast.FunctionDeclaration {
                        Ast.return_type;
                        Ast.function_name;
                        Ast.parameters;
                        Ast.statements;
                    }
            | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
            | None -> failwith "Unreachable code."
            )

        method private declarations =
            match Stream.peek stream with
            | Some {token = Eof} | None -> []
            | Some token ->
                let declaration = self#declaration in
                declaration :: self#declarations

        method private do_while_statement =
            self#eat Do;
            self#eat LeftCurlyBracket;
            let while_statements = self#statements in
            self#eat RightCurlyBracket;
            self#eat While;
            self#eat LeftParenthesis;
            let while_condition = self#expression in
            self#eat RightParenthesis;
            self#eat SemiColon;
            Ast.While { Ast.while_condition; Ast.while_statements }

        method private drop =
            Stream.junk stream

        method private eat token_to_eat =
            match Stream.peek stream with
            | Some {token} when token = token_to_eat -> self#drop
            | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position
            | None -> failwith "Unreachable code."

        method private else_if_statements =
            match Stream.npeek 2 stream with
            | [{token = Else}; {token = If}] ->
                    self#drop;
                    let condition_statements = self#if_condition_statements in
                    let next_if_statements = self#else_if_statements in
                    condition_statements :: next_if_statements
            | _ -> []

        method private equal_value =
            self#eat Equal;
            self#assignment_expression

        method private equality_expression =
            let expr1 = self#relational_expression in
            let rec equality_expressions expr1 =
                match Stream.peek stream with
                | Some {token = IsEqual} ->
                        self#drop;
                        let expr2 = self#relational_expression in
                        equality_expressions (Ast.Equals (expr1, expr2))
                | Some {token = NotEqual} ->
                        self#drop;
                        let expr2 = self#relational_expression in
                        equality_expressions (Ast.NotEqual (expr1, expr2))
                | _ -> expr1
            in equality_expressions expr1

        method private expression =
            let expr1 = self#assignment_expression in
            let rec comma_expression expr1 =
                match Stream.peek stream with
                | Some {token = Comma} ->
                        self#drop;
                        let expr2 = self#assignment_expression in
                        comma_expression (Ast.CommaExpression (expr1, expr2))
                | _ -> expr1
            in comma_expression expr1

        method private expression_statement =
            let expr = self#expression in
            self#eat SemiColon;
            Ast.Expression expr

        method private for_initialization =
            try
                match Stream.peek stream with
                | Some {token = Identifier identifier} ->
                        let _ = Hashtbl.find types identifier in
                        Ast.ForVariableDeclarations (self#variable_declarations)
                | _ -> failwith "Unreachable code."
            with Not_found ->
                Ast.ForExpression (self#expression)

        method private for_statement =
            self#eat For;
            self#eat LeftParenthesis;
            let for_init = self#for_initialization in
            self#eat SemiColon;
            let for_condition = self#expression in
            self#eat SemiColon;
            let for_increment = self#expression in
            self#eat RightParenthesis;
            self#eat LeftCurlyBracket;
            let for_statements = self#statements in
            self#eat RightCurlyBracket;
            Ast.For { Ast.for_init; Ast.for_condition; Ast.for_increment; Ast.for_statements }

        method private function_call =
            let file_position = self#current_position in
            let called_function_name = self#identifier in
            self#eat LeftParenthesis;
            let arguments = self#arguments in
            self#eat RightParenthesis;
            Ast.FunctionCall {Ast.called_function_name; Ast.arguments; Ast.file_position}

        method private identifier =
            match Stream.peek stream with
            | Some {token = Identifier identifier} ->
                    self#drop;
                    identifier
            | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
            | None -> failwith "Unreachable code."

        method private if_condition_statements =
            let if_condition = self#if_expression in
            let if_statements = self#bracketed_statements in
            { Ast.if_condition; Ast.if_statements }

        method private if_expression =
            self#eat If;
            self#eat LeftParenthesis;
            let if_condition = self#expression in
            self#eat RightParenthesis;
            if_condition

        method private if_statement =
            let condition_statements = self#if_condition_statements in
            let else_ifs = self#else_if_statements in
            let else_statements =
                match Stream.peek stream with
                | Some {token = Else} ->
                        self#drop;
                        self#eat LeftCurlyBracket;
                        let statements = self#statements in
                        self#eat RightCurlyBracket;
                        Some statements
                | _ -> None
            in
            Ast.If { Ast.else_ifs; Ast.else_statements; Ast.condition_statements }

        method private logical_and_expression =
            let expr1 = self#equality_expression in
            let rec logical_and_expressions expr1 =
                match Stream.peek stream with
                | Some { token = Ampersands } ->
                        self#drop;
                        let expr2 = self#equality_expression in
                        logical_and_expressions (Ast.LogicalAnd (expr1, expr2))
                | _ -> expr1
            in logical_and_expressions expr1

        method private logical_or_expression =
            let expr1 = self#logical_and_expression in
            let rec logical_or_expressions expr1 =
                match Stream.peek stream with
                | Some { token = Pipes } ->
                        self#drop;
                        let expr2 = self#logical_and_expression in
                        logical_or_expressions (Ast.LogicalOr (expr1, expr2))
                | _ -> expr1
            in logical_or_expressions expr1

        method private multiplicative_expression =
            let expr1 = self#unary_expression in
            let rec multiplicative_expressions expr1 =
                match Stream.peek stream with
                | Some {token = Star} ->
                        self#drop;
                        let expr2 = self#unary_expression in
                        multiplicative_expressions (Ast.Operation (Ast.Multiplication (expr1, expr2)))
                | Some {token = Slash} ->
                        self#drop;
                        let expr2 = self#unary_expression in
                        multiplicative_expressions (Ast.Operation (Ast.Division (expr1, expr2)))
                | Some {token = Modulo} ->
                        self#drop;
                        let expr2 = self#unary_expression in
                        multiplicative_expressions (Ast.Operation (Ast.Modulo (expr1, expr2)))
                | _ -> expr1
            in multiplicative_expressions expr1

        method private parameter =
            let parameter_type = self#typ in
            let parameter_name = self#identifier in
            let parameter_type = self#array_type parameter_type in
            {
                Ast.parameter_type;
                Ast.parameter_name;
            }

        method private parameters =
            match Stream.peek stream with
            | Some {token = RightParenthesis} ->
                    []
            | _ ->
                let parameter1 = self#parameter in
                let rec parameters' parameter1 =
                    (match Stream.peek stream with
                    | Some {token = Comma} ->
                            self#drop;
                            let parameter2 = self#parameter in
                            parameters' (List.append parameter1 [parameter2])
                    | _ -> parameter1
                    )
                in parameters' [parameter1]

        method parse =
            Ast.File self#declarations

        method private pointer_type typ =
            match Stream.peek stream with
            | Some {token = Star} ->
                self#drop;
                self#pointer_type (Ast.Pointer typ)
            | _ -> typ

        method private post_decrementation =
            let variable_name = self#identifier in
            self#eat MinusMinus;
            Ast.Decrement variable_name

        method private post_incrementation =
            let variable_name = self#identifier in
            self#eat PlusPlus;
            Ast.Increment variable_name

        method private postfix_expression =
            match Stream.npeek 2 stream with
            | [_; {token = LeftParenthesis}] -> self#function_call
            | [_; {token = LeftSquareBracket}] -> self#array_index
            | [_; {token = PlusPlus}] -> self#post_incrementation
            | [_; {token = MinusMinus}] -> self#post_decrementation
            | _ -> self#primary_expression

        method private primary_expression =
            match Stream.peek stream with
            | Some {token = LeftParenthesis} ->
                    self#drop;
                    let expr = self#expression in
                    self#eat RightParenthesis;
                    expr
            | Some {token = Float floating} ->
                    self#drop;
                    Ast.Float floating
            | Some {token = Int integer} ->
                    self#drop;
                    Ast.Int integer
            | Some {token = String str} ->
                    self#drop;
                    Ast.String str
            | Some {token = Character str} ->
                    self#drop;
                    Ast.Character str
            | Some {token = Identifier identifier} ->
                    self#drop;
                    Ast.Variable identifier
            | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position
            | None -> failwith "Unreachable code."

        method private relational_expression =
            let expr1 = self#additive_expression in
            let rec relational_expressions expr1 =
                match Stream.peek stream with
                | Some {token = Greater} ->
                        self#drop;
                        let expr2 = self#additive_expression in
                        relational_expressions (Ast.Greater (expr1, expr2))
                | Some {token = GreaterOrEqual} ->
                        self#drop;
                        let expr2 = self#additive_expression in
                        relational_expressions (Ast.GreaterOrEqual (expr1, expr2))
                | Some {token = Lesser} ->
                        self#drop;
                        let expr2 = self#additive_expression in
                        relational_expressions (Ast.Lesser (expr1, expr2))
                | Some {token = LesserOrEqual} ->
                        self#drop;
                        let expr2 = self#additive_expression in
                        relational_expressions (Ast.LesserOrEqual (expr1, expr2))
                | _ -> expr1
            in relational_expressions expr1

        method private return_statement =
            self#eat Return;
            let expr = self#expression in
            self#eat SemiColon;
            Ast.Return expr

        method private statement =
            match Stream.peek stream with
            | Some {token = Break} -> self#break_statement
            | Some {token = Return} -> self#return_statement
            | Some {token = Identifier identifier} ->
                    (try
                        let _ = Hashtbl.find types identifier in
                        self#variable_declaration_statement
                    with Not_found ->
                        self#expression_statement
                    )
            | Some {token = Const} ->
                    self#constant_declaration_statement
            | Some {token = Do} ->
                    self#do_while_statement
            | Some {token = For} ->
                    self#for_statement
            | Some {token = If} ->
                    self#if_statement
            | Some {token = Switch} ->
                    self#switch_statement
            | Some {token = While} ->
                    self#while_statement
            | Some ({token_position} as token) -> parse_error ("Unexpected token " ^ string_of_token token) token_position 
            | None -> failwith "Unreachable code."

        method private statements =
            match Stream.peek stream with
            | Some {token = RightCurlyBracket} -> []
            | Some {token = Case} -> []
            | Some {token = Default} -> []
            | _ ->
                    let statement = self#statement in
                    statement :: self#statements

        method private switch_statement =
            self#eat Switch;
            self#eat LeftParenthesis;
            let switch_expression = self#expression in
            self#eat RightParenthesis;
            self#eat LeftCurlyBracket;
            let switch_conditions = self#case_list in
            self#eat RightCurlyBracket;
            Ast.Switch { Ast.switch_expression; Ast.switch_conditions }

        method private ternary_expression = 
            let expr = self#logical_or_expression in
            match Stream.peek stream with
            | Some {token = QuestionMark} ->
                    self#drop;
                    let true_expression = self#expression in
                    self#eat Colon;
                    let false_expression = self#ternary_expression in
                    Ast.Ternary { Ast.ternary_condition = expr; Ast.true_expression; Ast.false_expression }
            | _ -> expr

        method private typ =
            let typ = self#identifier in
            self#pointer_type (Ast.Type typ)

        method private unary_expression =
            match Stream.peek stream with
            | Some {token = Minus} ->
                    self#drop;
                    let expr = self#unary_expression in
                    Ast.Negate expr
            | Some {token = Not} ->
                    self#drop;
                    let expr = self#unary_expression in
                    Ast.Not expr
            | _ -> self#postfix_expression

        method private variable_assignment variable_name =
            self#eat Equal;
            let variable_value = self#assignment_expression in
            Ast.Assignment {Ast.variable_name; Ast.variable_value}

        method private variable_declaration =
            let variable_type = self#typ in
            self#variable_declaration_without_type variable_type

        method private variable_declaration_statement =
            let declarations = self#variable_declarations in
            self#eat SemiColon;
            Ast.VariableDeclarations declarations

        method private variable_declaration_without_type variable_type =
            let variable_name = self#identifier in
            let variable_value = match Stream.peek stream with
                | Some {token = Equal} -> Some (self#equal_value)
                | _ -> None
            in
            {Ast.variable_type; Ast.variable_name; Ast.variable_value}

        method private variable_declarations =
            let declaration1 = self#variable_declaration in
            let {Ast.variable_type} = declaration1 in
            let rec variable_declarations' declaration1 =
                match Stream.peek stream with
                | Some {token = Comma} ->
                        self#drop;
                        let declaration2 = self#variable_declaration_without_type variable_type in
                        variable_declarations' (declaration2 :: declaration1)
                | _ -> List.rev declaration1
            in variable_declarations' [declaration1]

        method private while_statement =
            self#eat While;
            self#eat LeftParenthesis;
            let while_condition = self#expression in
            self#eat RightParenthesis;
            self#eat LeftCurlyBracket;
            let while_statements = self#statements in
            self#eat RightCurlyBracket;
            Ast.While { Ast.while_condition; Ast.while_statements }
end

let rec print_tokens stream = match Stream.peek stream with
    | Some token -> trace token; Stream.junk stream; print_tokens stream
    | None -> ()

let parse filename =
    let lexer = new lexer filename in
    let parsr = new parsr lexer in
    let ast = parsr#parse in
    lexer#close;
    ast
