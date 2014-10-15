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

let return_values = Stack.create ()

let variables = Hashtbl.create 10

let rec putc = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [Character character] -> print_string (String.make 1 character)
    | [Variable variable] -> (match Hashtbl.find variables variable with
        | Some Character character -> putc [Character character]
        | _ -> print_endline "One integer parameter is expected."
    )
    | _ -> print_endline "One integer parameter is expected."

let puti = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [Int integer] -> print_int integer
    | [Variable variable] -> (match Hashtbl.find variables variable with
        | Some Int integer -> print_int integer
        | _ -> print_endline "One integer parameter is expected."
    )
    | _ -> print_endline "One integer parameter is expected."

let puts = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [String string_literal] -> print_endline string_literal
    | _ -> print_endline "One string parameter is expected."

let rec add_variables_from_arguments parameters arguments = match (parameters, arguments) with
    | ([], []) -> ()
    | ([], _) -> print_endline "Too much arguments."
    | (_, []) -> print_endline "Missing arguments."
    | ({parameter_type; parameter_name} :: next_parameters, argument :: next_arguments) ->
            Hashtbl.add variables parameter_name (Some argument);
            add_variables_from_arguments next_parameters next_arguments

let rec compare_expression expression1 expression2 = match expression1 with
    | Character character1 -> (match expression2 with
        | Character character2 -> (int_of_char character1) - (int_of_char character2)
    )
    | Int integer1 -> (match expression2 with
        | Int integer2 -> integer1 - integer2
        | Variable variable_name -> (match Hashtbl.find variables variable_name with
            | Some expression -> compare_expression expression1 expression
        )
    )
    | Variable variable_name -> (match Hashtbl.find variables variable_name with
        | Some expression -> compare_expression expression expression2
    )

let is_true = function
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

let rec execute_expression = function
    | Assignment { variable_name; variable_value } ->
            Hashtbl.add variables variable_name (Some (execute_expression variable_value));
            variable_value
    | Character _ | Int _ as value -> value
    | FunctionCall { called_function_name = "putc"; arguments = parameters } ->
            putc parameters;
            Void
    | FunctionCall { called_function_name = "puti"; arguments = parameters } ->
            puti parameters;
            Void
    | FunctionCall { called_function_name = "puts"; arguments = parameters } ->
            puts parameters;
            Void
    | FunctionCall { called_function_name; arguments } -> (match Hashtbl.find functions called_function_name with
            | Some (FunctionDefinition {return_type; function_name; parameters; statements}) ->
                    add_variables_from_arguments parameters arguments;
                    List.iter execute_statement statements;
                    (match Stack.pop return_values with
                    | Int _ as value -> value
                    | Variable variable_name ->
                            (match Hashtbl.find variables variable_name with
                            | Some expression -> expression
                            | None -> print_endline ("No variable named " ^ variable_name ^ "."); Void
                            )
                    )
            | None ->
                    print_endline ("The function " ^ called_function_name ^ " does not exist.");
                    Void
    )
    | Increment variable_name -> (match Hashtbl.find variables variable_name with
        | Some Int integer ->
                Hashtbl.replace variables variable_name (Some (Int (integer + 1)));
                Int integer
        | Some Character character ->
                let new_character = char_of_int (int_of_char character + 1) in
                Hashtbl.replace variables variable_name (Some (Character (new_character)));
                Character character
        | _ ->
                print_endline "Cannot increment this kind of value.";
                Void
    )
    | Void -> Void

and execute_statement = function
    | ConstantDeclaration { constant_type; constant_name; constant_value } ->
            Hashtbl.add variables constant_name (Some constant_value)
    | DoWhile { do_while_condition; do_while_statements } as do_while_statement -> 
        List.iter execute_statement do_while_statements;
        if is_true do_while_condition then execute_statement do_while_statement
    | Expression expression -> execute_expression expression; ()
    | For ({ for_init; for_condition; for_increment; for_statements } as for_statement) ->
            execute_for_initialization for_init;
            if is_true for_condition then (
                List.iter execute_statement for_statements;
                execute_expression for_increment;
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
        | Some value -> Hashtbl.add variables variable_name (Some (execute_expression value))
        | None -> Hashtbl.add variables variable_name None

and execute_for_initialization = function
    | ForExpression expression -> execute_expression expression; ()
    | ForVariableDeclaration variable_declaration -> execute_statement (VariableDeclaration variable_declaration)

let execute = function
    | FunctionDefinition { return_type; function_name; parameters; statements } as fnctn ->
            Hashtbl.add functions function_name (Some fnctn)

let interpret filename =
    let ast = FileParser.parse filename in
    List.iter execute ast;
    execute_expression (FunctionCall { called_function_name = "main"; arguments = [] }) ;
    ()
