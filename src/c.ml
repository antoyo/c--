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

type expr =
    | Equals of expr * expr
    | Greater of expr * expr
    | GreaterOrEqual of expr * expr
    | Int of int
    | String of string
    | Variable of string

type assignment = {
    variable_name: string;
    variable_value: expr;
}

type constant_declaration = {
    constant_type: string;
    constant_name: string;
    constant_value: expr;
}

type function_call = {
    called_function_name: string;
    arguments: expr list;
}

type value_type = {
    base_type: string;
    indirection_level: int;
}

type parameter = {
    parameter_type: value_type;
    parameter_name: string;
}

type variable_declaration = {
    variable_type: string;
    variable_name: string;
    variable_value: expr option;
}

type statement =
    | Assignment of assignment
    | ConstantDeclaration of constant_declaration
    | FunctionCall of function_call
    | If of if_statement
    | Increment of string
    | Return of expr
    | VariableDeclaration of variable_declaration

and if_statement = {
    else_statements: statement list option;
    if_condition: expr;
    if_statements: statement list;
}

type function_definition = {
    return_type: string;
    function_name: string;
    parameters: parameter list;
    statements: statement list;
}

type definition =
    | FunctionDefinition of function_definition
