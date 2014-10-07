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

%token <int> INT
%token <string> ID
%token <string> STRING
%token COMMA
%token CONSTANT
%token ELSE
%token EOF
%token EQUAL
%token EQUALS
%token GREATER
%token GREATER_OR_EQUAL
%token IF
%token LEFT_CURLY_BRACKET
%token LEFT_PARENTHESIS
%token LEFT_SQUARE_BRACKET
%token RETURN
%token PLUS_PLUS
%token RIGHT_CURLY_BRACKET
%token RIGHT_PARENTHESIS
%token RIGHT_SQUARE_BRACKET
%token SEMI_COLON
%token STAR

%start <C.definition list> prog
%{
    open C
%}
%%

prog:
    | i = definitions { i }

definitions:
    | return_type = ID; function_name = ID; LEFT_PARENTHESIS; parameters = params; RIGHT_PARENTHESIS; LEFT_CURLY_BRACKET;
            statements = list(instruction); RIGHT_CURLY_BRACKET; next_definitions = definitions
        { FunctionDefinition {return_type; function_name; parameters; statements} :: next_definitions }
    | EOF { [] }

instruction:
    | IF; LEFT_PARENTHESIS; if_condition = expr; RIGHT_PARENTHESIS; LEFT_CURLY_BRACKET;
                if_statements = list(instruction); RIGHT_CURLY_BRACKET;
            ELSE; LEFT_CURLY_BRACKET;
                else_statements = list(instruction); RIGHT_CURLY_BRACKET
        { If { if_condition; if_statements; else_statements = Some else_statements } }
    | IF; LEFT_PARENTHESIS; if_condition = expr; RIGHT_PARENTHESIS; LEFT_CURLY_BRACKET;
            if_statements = list(instruction); RIGHT_CURLY_BRACKET
        { If { if_condition; if_statements; else_statements = None } }
    | variable_type = ID; variable_name = ID; EQUAL; variable_value = expr; SEMI_COLON { VariableDeclaration {variable_type; variable_name; variable_value = Some variable_value} }
    | variable_type = ID; variable_name = ID; SEMI_COLON { VariableDeclaration {variable_type; variable_name; variable_value = None} }
    | CONSTANT; constant_type = ID; constant_name = ID; EQUAL; constant_value = expr; SEMI_COLON { ConstantDeclaration {constant_type; constant_name; constant_value} }
    | variable_name = ID; EQUAL; variable_value = expr; SEMI_COLON { Assignment {variable_name; variable_value} }
    | variable_name = ID; PLUS_PLUS; SEMI_COLON { Increment variable_name }
    | called_function_name = ID; LEFT_PARENTHESIS; arguments = arguments; RIGHT_PARENTHESIS; SEMI_COLON
        { FunctionCall {called_function_name; arguments} }
    | RETURN; expression = expr; SEMI_COLON { Return expression }

expr:
    | name = ID { Variable name }
    | integer = INT { Int integer }
    | string_literal = STRING { String string_literal }
    | expression1 = expr; EQUALS; expression2 = expr { Equals (expression1, expression2) }
    | expression1 = expr; GREATER; expression2 = expr { Greater (expression1, expression2) }
    | expression1 = expr; GREATER_OR_EQUAL; expression2 = expr { GreaterOrEqual (expression1, expression2) }

arguments:
    parameters = separated_list(COMMA, expr) { parameters }

params:
    parameters = separated_list(COMMA, param) { parameters }

param:
    | base_type = ID; parameter_name = ID { {parameter_type = {base_type; indirection_level = 0}; parameter_name} }
    | base_type = ID; STAR; parameter_name = ID; LEFT_SQUARE_BRACKET; RIGHT_SQUARE_BRACKET { {parameter_type = {base_type; indirection_level = 2}; parameter_name} }
