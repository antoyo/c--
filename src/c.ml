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
    | Assignment of assignment
    | Character of char
    | Equals of expr * expr
    | FunctionCall of function_call
    | Greater of expr * expr
    | GreaterOrEqual of expr * expr
    | Increment of string
    | Indirection of indirection
    | Int of int
    | Lesser of expr * expr
    | LesserOrEqual of expr * expr
    | NotEqual of expr * expr
    | String of string
    | Variable of string
    | Void

and assignment = {
    variable_name: string;
    variable_value: expr;
}

and function_call = {
    called_function_name: string;
    arguments: expr list;
}

and indirection = {
    indirection_name: string;
    indirection_index: expr;
}

type typ =
    | Type of string
    | Pointer of typ

type constant_declaration = {
    constant_type: typ;
    constant_name: string;
    constant_value: expr;
}

type parameter = {
    parameter_type: typ;
    parameter_name: string;
}

type variable_declaration = {
    variable_type: typ;
    variable_name: string;
    variable_value: expr option;
}

type for_initialization =
    | ForVariableDeclaration of variable_declaration
    | ForExpression of expr

type statement =
    | ConstantDeclaration of constant_declaration
    | DoWhile of do_while_statement
    | Expression of expr
    | For of for_statement
    | If of if_statement
    | Return of expr
    | VariableDeclaration of variable_declaration
    | While of while_statement

and do_while_statement = {
    do_while_condition: expr;
    do_while_statements: statement list;
}

and for_statement = {
    for_init: for_initialization;
    for_condition: expr;
    for_increment: expr;
    for_statements: statement list;
}

and if_statement = {
    else_statements: statement list option;
    if_condition: expr;
    if_statements: statement list;
}

and while_statement = {
    while_condition: expr;
    while_statements: statement list;
}

type function_definition = {
    return_type: typ;
    function_name: string;
    parameters: parameter list;
    statements: statement list;
}

type definition =
    | FunctionDefinition of function_definition
