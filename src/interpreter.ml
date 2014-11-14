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
 * TODO: Create a JIT compiler.
 *)

open C

type statement_type =
    | Broke
    | Continued
    | NormalStatement

let functions = Hashtbl.create 10

let add_function name value = Hashtbl.add functions name (Some value)

let declare_function name = Hashtbl.add functions name None

let get_function name = Hashtbl.find functions name

let return_values = Stack.create ()

let variables = Hashtbl.create 10

let declare_variable_without_value name = Hashtbl.add variables name None

let get_variable name = Hashtbl.find variables name

let set_variable_value name value = Hashtbl.replace variables name (Some value)

let rec add_variables_from_arguments parameters arguments =
    match (parameters, arguments) with
    | ([], []) -> ()
    | ([], _) -> print_endline "Too much arguments."
    | (_, []) -> print_endline "Missing arguments."
    | ({parameter_type; parameter_name} :: next_parameters, argument :: next_arguments) ->
            set_variable_value parameter_name argument;
            add_variables_from_arguments next_parameters next_arguments

let change_variable variable_name operation =
    match get_variable variable_name with
        | Some Int integer ->
                set_variable_value variable_name (Int (operation integer));
                Int integer
        | Some Character character ->
                let new_character = char_of_int (operation (int_of_char character)) in
                set_variable_value variable_name (Character (new_character));
                Character character
        | _ ->
                print_endline "Cannot increment this kind of value.";
                Void

let declare_constant { constant_type; constant_name; constant_value } =
    set_variable_value constant_name constant_value

let execute_indirection { indirection_name; indirection_index } =
    (match get_variable indirection_name with
        | Some String string_value -> (match indirection_index with
            | Int integer -> Character (String.get string_value integer)
        )
    )

let rec execute_expression = function
    | Assignment { variable_name; variable_value } ->
            set_variable_value variable_name (execute_expression variable_value);
            variable_value
    | AssignmentOperation assignment_operation -> execute_assignment_operation assignment_operation
    | Character _ | Int _ | String _ as value -> value
    | Decrement variable_name -> change_variable variable_name (fun value -> value - 1)
    | FunctionCall { called_function_name = "putc"; arguments = parameters } -> putc parameters; Void
    | FunctionCall { called_function_name = "puti"; arguments = parameters } -> puti parameters; Void
    | FunctionCall { called_function_name = "puts"; arguments = parameters } -> puts parameters; Void
    | FunctionCall function_call -> call_function function_call 
    | Increment variable_name -> change_variable variable_name (fun value -> value + 1)
    | Indirection indirection -> execute_indirection indirection
    | Operation operation -> execute_operation operation
    | Variable variable -> (match get_variable variable with
        | Some value -> execute_expression value
        | None -> print_endline ("The variable " ^ variable ^ " is declared but does not have any value."); Void
        | exception Not_found -> print_endline ("The variable "  ^ variable ^ " is not declared."); Void
    )
    | Void -> Void

and call_function { called_function_name; arguments } =
    match get_function called_function_name with
            | Some (FunctionDefinition {return_type; function_name; parameters; statements}) ->
                    add_variables_from_arguments parameters arguments;
                    List.iter execute_statement statements;
                    execute_expression (Stack.pop return_values)
            | None ->
                    print_endline ("The function " ^ called_function_name ^ " does not exist.");
                    Void

and declare_variable { variable_type; variable_name; variable_value } =
    match variable_value with
        | Some value -> set_variable_value variable_name (execute_expression value)
        | None -> declare_variable_without_value variable_name

and execute_operation operation =
    let execute_operation expression1 expression2 operator =
        let expression1 = execute_expression expression1 in
        let expression2 = execute_expression expression2 in
        (match (expression1, expression2) with
            | (Int value1, Int value2) -> Int (operator value1 value2)
        )
    in
    (match operation with
        | Addition (expression1, expression2) -> execute_operation expression1 expression2 (+)
        | Subtraction (expression1, expression2) -> execute_operation expression1 expression2 (-)
        | Multiplication (expression1, expression2) -> execute_operation expression1 expression2 ( * )
        | Division (expression1, expression2) -> execute_operation expression1 expression2 (/)
        | Modulo (expression1, expression2) -> execute_operation expression1 expression2 (mod)
    )

and execute_assignment_operation assignment_operation =
    let (variable_name, new_value) = (match assignment_operation with
        | AssignAdd (variable_name, value) -> (variable_name, execute_operation (Addition (Variable variable_name, value)))
        | AssignSubtract (variable_name, value) -> (variable_name, execute_operation (Subtraction (Variable variable_name, value)))
        | AssignMultiply (variable_name, value) -> (variable_name, execute_operation (Multiplication (Variable variable_name, value)))
        | AssignDivide (variable_name, value) -> (variable_name, execute_operation (Division (Variable variable_name, value)))
        | AssignModulo (variable_name, value) -> (variable_name, execute_operation (Modulo (Variable variable_name, value)))
    )
    in
    set_variable_value variable_name new_value;
    new_value

and execute_statement = function
    | ConstantDeclaration constant_declaration -> declare_constant constant_declaration
    | DoWhile _ as do_while_statement -> execute_do_while do_while_statement
    | Expression expression -> let _ = execute_expression expression in ()
    | For _ as for_statement -> execute_for for_statement
    | If if_statement -> execute_if if_statement
    | Switch switch -> execute_switch switch
    | While _ as while_statement -> execute_while while_statement
    | Return value -> Stack.push value return_values
    | VariableDeclaration variable_declaration -> declare_variable variable_declaration

and compare_expression expression1 expression2 = 
    let value1 = execute_expression expression1 in
    let value2 = execute_expression expression2 in
    (match (value1, value2) with
        | (Character character1, Character character2) -> (int_of_char character1) - (int_of_char character2)
        | (Int integer1, Int integer2) -> integer1 - integer2
    )

and evaluate_cases expression force = function
    | [] -> ()
    | case :: next_cases -> (match case with
        | Case { case_condition; case_instructions } ->
            if force || is_true (Equals (expression, case_condition)) then (
                match execute_until_break case_instructions with
                | Broke -> ()
                | _ -> evaluate_cases expression true next_cases
            )
            else
                evaluate_cases expression false next_cases
        | Default statements -> List.iter execute_statement statements
    )

and execute_do_while do_while_statement =
    match do_while_statement with
    | DoWhile { do_while_condition; do_while_statements } ->
        List.iter execute_statement do_while_statements;
        if is_true do_while_condition then execute_statement do_while_statement

and execute_for for_statement =
    match for_statement with
    | For ({ for_init; for_condition; for_increment; for_statements } as for_statement) ->
            execute_for_initialization for_init;
            if is_true for_condition then (
                List.iter execute_statement for_statements;
                let _ = execute_expression for_increment in
                execute_statement (For {for_statement with for_init = ForExpression Void})
            )

and execute_for_initialization = function
    | ForExpression expression -> let _ = execute_expression expression in ()
    | ForVariableDeclaration variable_declaration -> execute_statement (VariableDeclaration variable_declaration)

and execute_if { else_statements; if_condition; if_statements } = if is_true if_condition then
            List.iter execute_statement if_statements
        else (
            match else_statements with
            | Some statements -> List.iter execute_statement statements
            | None -> ()
        )

and execute_switch {switch_expression; switch_conditions} =
    evaluate_cases switch_expression false switch_conditions

and execute_until_break = function
    | [] -> NormalStatement
    | Break :: _ -> Broke
    | instruction :: next_instructions -> (
        execute_statement instruction;
        execute_until_break next_instructions
    )

and execute_while while_statement =
    match while_statement with
    | While { while_condition; while_statements } as while_statement ->
            if is_true while_condition then (
                List.iter execute_statement while_statements;
                execute_statement while_statement
            )

and is_true = function
    | Equals (expression1, expression2) ->
            let result = compare_expression expression1 expression2 in
            result = 0
    | Int integer -> integer <> 0
    | Greater (expression1, expression2) ->
            let result = compare_expression expression1 expression2 in
            result > 0
    | GreaterOrEqual (expression1, expression2) ->
            let result = compare_expression expression1 expression2 in
            result >= 0
    | Lesser (expression1, expression2) ->
            let result = compare_expression expression1 expression2 in
            result < 0
    | LesserOrEqual (expression1, expression2) ->
            let result = compare_expression expression1 expression2 in
            result <= 0
    | NotEqual (expression1, expression2) ->
            let result = compare_expression expression1 expression2 in
            result <> 0

and putc = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [expression] -> (match execute_expression expression with
        | Character character -> print_string (String.make 1 character)
        | _ -> print_endline "One character parameter is expected."
    )
    | _ -> print_endline "One character parameter is expected."

and puti = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [expression] -> (match execute_expression expression with
        | Int integer -> print_int integer
        | _ -> print_endline "One integer parameter is expected."
    )
    | _ -> print_endline "One integer parameter is expected."

and puts = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [expression] -> (match execute_expression expression with
        | String string_literal -> print_endline string_literal
        | _ -> print_endline "One string parameter is expected."
    )
    | _ -> print_endline "One string parameter is expected."

let execute = function
    | FunctionDefinition { return_type; function_name; parameters; statements } as fnctn ->
            add_function function_name fnctn

open Lexer

let print_token token = match token with
    | (token, _) -> (match token with
        | Eof -> print_endline "eof"
        | LeftCurlyBracket -> print_endline "{"
        | RightCurlyBracket -> print_endline "}"
        | LeftParenthesis -> print_endline "("
        | RightParenthesis -> print_endline ")"
        | LeftSquareBracket -> print_endline "["
        | RightSquareBracket -> print_endline "]"
        | Colon -> print_endline ":"
        | SemiColon -> print_endline ";"
        | Comma -> print_endline ","
        | Greater -> print_endline ">"
        | GreaterOrEqual -> print_endline ">="
        | Lesser -> print_endline "<"
        | LesserOrEqual -> print_endline "<="
        | NotEqual -> print_endline "!="
        | IsEqual -> print_endline "=="
        | Equal -> print_endline "="
        | Plus -> print_endline "+"
        | PlusEqual -> print_endline "+="
        | Minus -> print_endline "-"
        | MinusEqual -> print_endline "-="
        | Times -> print_endline "*"
        | TimesEqual -> print_endline "*="
        | Divide -> print_endline "/"
        | DivideEqual -> print_endline "/="
        | Modulo -> print_endline "%"
        | ModuloEqual -> print_endline "%="
        | Int n -> print_int n; print_endline ""
        | Float n -> print_float n; print_endline ""
        | Identifier i -> print_endline i
        | String str -> print_char '"'; print_string str; print_char '"'; print_endline ""
        | Character character -> print_char '\''; print_char character; print_char '\''; print_endline ""
        | If -> print_endline "if statement"
        | Else -> print_endline "else statement"
        | Return -> print_endline "return statement"
        | Const -> print_endline "const declaration"
        | While -> print_endline "while statement"
        | For -> print_endline "for statement"
        | Do -> print_endline "do while statement"
        | Switch -> print_endline "switch statement"
        | Case -> print_endline "case label"
        | Break -> print_endline "break statement"
        | Default -> print_endline "default label"
    )

let interpret filename =
    let ast = FileParser.parse filename in
    List.iter print_token ast
    (*List.iter execute ast;
    let _ = execute_expression (FunctionCall { called_function_name = "main"; arguments = [] }) in
    ()*)
