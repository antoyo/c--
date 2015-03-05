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
    | AssignmentOperation of assignment_operation
    | Array of expr array
    | Character of char
    | Decrement of string
    | Equals of expr * expr
    | Float of float
    | FunctionCall of function_call
    | Greater of expr * expr
    | GreaterOrEqual of expr * expr
    | Increment of string
    | Indirection of indirection
    | Int of int
    | Lesser of expr * expr
    | LesserOrEqual of expr * expr
    | Negate of expr
    | NotEqual of expr * expr
    | Operation of operation
    | String of string
    | Variable of string
    | Void

and assignment = {
    variable_name: string;
    variable_value: expr;
}

and assignment_operation =
    | AssignAdd of string * expr
    | AssignSubtract of string * expr
    | AssignMultiply of string * expr
    | AssignDivide of string * expr
    | AssignModulo of string * expr

and function_call = {
    called_function_name: string;
    arguments: expr list;
}

and indirection = {
    indirection_name: string;
    indirection_index: expr;
}

and operation =
    | Addition of expr * expr
    | Subtraction of expr * expr
    | Multiplication of expr * expr
    | Division of expr * expr
    | Modulo of expr * expr

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
    | Break
    | ConstantDeclaration of constant_declaration
    | DoWhile of do_while_statement
    | Expression of expr
    | For of for_statement
    | If of if_statement
    | Return of expr
    | Switch of switch_statement
    | VariableDeclaration of variable_declaration
    | While of while_statement

and case = {
    case_condition: expr;
    case_instructions: statement list;
}

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

and if_condition_statements = {
    if_condition: expr;
    if_statements: statement list;
}

and if_statement = {
    else_ifs: if_condition_statements list;
    else_statements: statement list option;
    condition_statements: if_condition_statements;
}

and switch_condition =
    | Case of case
    | Default of statement list

and switch_statement = {
    switch_expression: expr;
    switch_conditions: switch_condition list;
}

and while_statement = {
    while_condition: expr;
    while_statements: statement list;
}

type function_declaration = {
    return_type: typ;
    function_name: string;
    parameters: parameter list;
    statements: statement list;
}

type declaration =
    | GlobalConstantDeclaration of constant_declaration
    | FunctionDeclaration of function_declaration
    | FunctionPrototype of function_declaration
    | GlobalVariableDeclaration of variable_declaration
