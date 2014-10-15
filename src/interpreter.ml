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
 * TODO: Create a binding for libjit.
 * TODO: Implement a JIT compiler.
 *)

open C

let functions = Hashtbl.create 10

let add_function name value = Hashtbl.add functions name (Some value)

let declare_function name = Hashtbl.add functions name None

let get_function name = Hashtbl.find functions name

let return_values = Stack.create ()

let variables = Hashtbl.create 10

let declare_variable name = Hashtbl.add variables name None

let get_variable name = Hashtbl.find variables name

let set_variable_value name value = Hashtbl.replace variables name (Some value)

let puti = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [Int integer] -> print_int integer
    | [Variable variable] -> (match get_variable variable with
        | Some Int integer -> print_int integer
        | _ -> print_endline "One integer parameter is expected."
    )
    | _ -> print_endline "One integer parameter is expected."

let puts = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [String string_literal] -> print_endline string_literal
    | [Variable variable] -> (match get_variable variable with
        | Some (String string_value) -> print_endline string_value
    )
    | _ -> print_endline "One string parameter is expected."

let rec add_variables_from_arguments parameters arguments = match (parameters, arguments) with
    | ([], []) -> ()
    | ([], _) -> print_endline "Too much arguments."
    | (_, []) -> print_endline "Missing arguments."
    | ({parameter_type; parameter_name} :: next_parameters, argument :: next_arguments) ->
            set_variable_value parameter_name argument;
            add_variables_from_arguments next_parameters next_arguments

let rec execute_expression = function
    | Assignment { variable_name; variable_value } ->
            set_variable_value variable_name (execute_expression variable_value);
            variable_value
    | Character _ | Int _ | String _ as value -> value
    | FunctionCall { called_function_name = "putc"; arguments = parameters } ->
            putc parameters;
            Void
    | FunctionCall { called_function_name = "puti"; arguments = parameters } ->
            puti parameters;
            Void
    | FunctionCall { called_function_name = "puts"; arguments = parameters } ->
            puts parameters;
            Void
    | FunctionCall { called_function_name; arguments } -> (match get_function called_function_name with
            | Some (FunctionDefinition {return_type; function_name; parameters; statements}) ->
                    add_variables_from_arguments parameters arguments;
                    List.iter execute_statement statements;
                    (match Stack.pop return_values with
                    | Int _ as value -> value
                    | Variable variable_name ->
                            (match get_variable variable_name with
                            | Some expression -> expression
                            | None -> print_endline ("No variable named " ^ variable_name ^ "."); Void
                            )
                    )
            | None ->
                    print_endline ("The function " ^ called_function_name ^ " does not exist.");
                    Void
    )
    | Increment variable_name -> (match get_variable variable_name with
        | Some Int integer ->
                set_variable_value variable_name (Int (integer + 1));
                Int integer
        | Some Character character ->
                let new_character = char_of_int (int_of_char character + 1) in
                set_variable_value variable_name (Character (new_character));
                Character character
        | _ ->
                print_endline "Cannot increment this kind of value.";
                Void
    )
    | Indirection { indirection_name; indirection_index } -> (match get_variable indirection_name with
        | Some String string_value -> (match indirection_index with
            | Int integer -> Character (String.get string_value integer)
        )
    )
    | Variable variable -> (match get_variable variable with
        | Some value -> execute_expression value
        | None -> print_endline "This variable is not declared."; Void
    )
    | Void -> Void

and execute_statement = function
    | ConstantDeclaration { constant_type; constant_name; constant_value } ->
            set_variable_value constant_name constant_value
    | DoWhile { do_while_condition; do_while_statements } as do_while_statement -> 
        List.iter execute_statement do_while_statements;
        if is_true do_while_condition then execute_statement do_while_statement
    | Expression expression -> let _ = execute_expression expression in ()
    | For ({ for_init; for_condition; for_increment; for_statements } as for_statement) ->
            execute_for_initialization for_init;
            if is_true for_condition then (
                List.iter execute_statement for_statements;
                let _ = execute_expression for_increment in
                execute_statement (For {for_statement with for_init = ForExpression Void})
            )
    | If { else_statements; if_condition; if_statements } -> if is_true if_condition then
            List.iter execute_statement if_statements
        else (
            match else_statements with
            | Some statements -> List.iter execute_statement statements
            | None -> ()
        )
    | While { while_condition; while_statements } as while_statement -> if is_true while_condition then (
        List.iter execute_statement while_statements;
        execute_statement while_statement
    )
    | Return value ->
            Stack.push value return_values
    | VariableDeclaration { variable_type; variable_name; variable_value } -> match variable_value with
        | Some value -> set_variable_value variable_name (execute_expression value)
        | None -> declare_variable variable_name

and execute_for_initialization = function
    | ForExpression expression -> let _ = execute_expression expression in ()
    | ForVariableDeclaration variable_declaration -> execute_statement (VariableDeclaration variable_declaration)

and compare_expression expression1 expression2 = match expression1 with
    | Character character1 -> (match expression2 with
        | Character character2 -> (int_of_char character1) - (int_of_char character2)
        | Indirection _ as indirection -> compare_expression expression1 (execute_expression indirection)
    )
    | Indirection _ as indirection -> compare_expression (execute_expression indirection) expression2
    | Int integer1 -> (match expression2 with
        | Int integer2 -> integer1 - integer2
        | Variable variable_name -> (match get_variable variable_name with
            | Some expression -> compare_expression expression1 expression
        )
    )
    | Variable variable_name -> (match get_variable variable_name with
        | Some expression -> compare_expression expression expression2
    )

and is_true = function
    | Equals (expression1, expression2) ->
            let result = compare_expression expression1 expression2 in
            result = 0
    | Int integer -> integer != 0
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
            result != 0

and putc = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [Character character] -> print_string (String.make 1 character)
    | [Indirection _ as indirection] -> putc [execute_expression indirection]
    | [Variable variable] -> (match get_variable variable with
        | Some Character character -> putc [Character character]
        | _ -> print_endline "One character parameter is expected."
    )
    | _ -> print_endline "One character parameter is expected."

let execute = function
    | FunctionDefinition { return_type; function_name; parameters; statements } as fnctn ->
            add_function function_name fnctn

let interpret filename =
    let ast = FileParser.parse filename in
    List.iter execute ast;
    let _ = execute_expression (FunctionCall { called_function_name = "main"; arguments = [] }) in
    ()
