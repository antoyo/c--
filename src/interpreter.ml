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
 * TODO: Put this code on git.
 * TODO: Assignment, increment is an expression.
 * TODO: An expression can be a statement.
 * TODO: Refactor the code in the parser (if could be another part).
 *
 * TODO: Create a binding for libjit.
 * TODO: Implement a JIT compiler.
 *)

open C

let variables = Hashtbl.create 10

let puts = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [String string_literal] -> print_string string_literal
    | _ -> print_endline "One string parameter is expected."

let puti = function
    | _ :: _ :: [] -> print_endline "Too much parameter."
    | [Int integer] -> print_int integer
    | [Variable variable] -> (match Hashtbl.find variables variable with
        | Some Int integer -> print_int integer
        | _ -> print_endline "One integer parameter is expected."
    )
    | _ -> print_endline "One integer parameter is expected."

let rec compare_expression expression1 expression2 = match expression1 with
    | Int integer1 -> (match expression2 with
        | Int integer2 -> integer1 - integer2
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

let rec execute_statement = function
    | Assignment { variable_name; variable_value } -> Hashtbl.add variables variable_name (Some variable_value)
    | ConstantDeclaration { constant_type; constant_name; constant_value } ->
            Hashtbl.add variables constant_name (Some constant_value)
    | FunctionCall { called_function_name = "puts"; arguments = parameters } -> puts parameters
    | FunctionCall { called_function_name = "puti"; arguments = parameters } -> puti parameters
    | FunctionCall { called_function_name; arguments } -> print_endline ("The function " ^ called_function_name ^ " does not exist.")
    | If { else_statements; if_condition; if_statements } -> if is_true if_condition then
            List.iter execute_statement if_statements
        else (
            match else_statements with
            | Some statements -> List.iter execute_statement statements
            | None -> ()
        )
    | Increment variable_name -> (match Hashtbl.find variables variable_name with
        | Some Int integer -> Hashtbl.replace variables variable_name (Some (Int (integer + 1)))
        | _ -> print_endline "Cannot increment this kind of value."
    )
    | Return value -> ()
    | VariableDeclaration { variable_type; variable_name; variable_value } -> match variable_value with
        | Some value -> Hashtbl.add variables variable_name (Some value)
        | None -> Hashtbl.add variables variable_name None

let execute = function
    | FunctionDefinition { return_type; function_name; parameters; statements } ->
            List.iter execute_statement statements

let interpret filename =
    let ast = FileParser.parse filename in
    List.iter execute ast
