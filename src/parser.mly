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

%token <char> CHARACTER
%token <int> INT
%token <string> ID
%token <string> STRING
%token BREAK
%token CASE
%token COLON
%token COMMA
%token CONSTANT
%token DEFAULT
%token DO
%token ELSE
%token EOF
%token EQUAL
%token EQUALS
%token FOR
%token GREATER
%token GREATER_OR_EQUAL
%token IF
%token LEFT_CURLY_BRACKET
%token LEFT_PARENTHESIS
%token LEFT_SQUARE_BRACKET
%token LESSER
%token LESSER_OR_EQUAL
%token MINUS
%token MINUS_EQUAL
%token MINUS_MINUS
%token NOT_EQUAL
%token PERCENT
%token PERCENT_EQUAL
%token PLUS
%token PLUS_EQUAL
%token PLUS_PLUS
%token RETURN
%token RIGHT_CURLY_BRACKET
%token RIGHT_PARENTHESIS
%token RIGHT_SQUARE_BRACKET
%token SEMI_COLON
%token SLASH
%token SLASH_EQUAL
%token STAR
%token STAR_EQUAL
%token SWITCH
%token WHILE

%start <C.definition list> prog
%{
    open C
%}
%%

arguments:
    parameters = separated_list(COMMA, expr) { parameters }

assign_oper:
    | variable_name = ID; PLUS_EQUAL; variable_value = expr { AssignAdd (variable_name, variable_value) }
    | variable_name = ID; MINUS_EQUAL; variable_value = expr { AssignSubtract (variable_name, variable_value) }
    | variable_name = ID; STAR_EQUAL; variable_value = expr { AssignMultiply (variable_name, variable_value) }
    | variable_name = ID; SLASH_EQUAL; variable_value = expr { AssignDivide (variable_name, variable_value) }
    | variable_name = ID; PERCENT_EQUAL; variable_value = expr { AssignModulo (variable_name, variable_value) }

case:
    | CASE; case_condition = expr; COLON; case_instructions = list(instruction)
        { Case { case_condition; case_instructions } }
    | DEFAULT; COLON; instructions = list(instruction)
        { Default instructions }

condition:
    | expression1 = expr; EQUALS; expression2 = expr { Equals (expression1, expression2) }
    | expression1 = expr; GREATER; expression2 = expr { Greater (expression1, expression2) }
    | expression1 = expr; GREATER_OR_EQUAL; expression2 = expr { GreaterOrEqual (expression1, expression2) }
    | expression1 = expr; LESSER; expression2 = expr { Lesser (expression1, expression2) }
    | expression1 = expr; LESSER_OR_EQUAL; expression2 = expr { LesserOrEqual (expression1, expression2) }
    | expression1 = expr; NOT_EQUAL; expression2 = expr { NotEqual (expression1, expression2) }

constant_statement:
    | CONSTANT; constant_type = typ; constant_name = ID; EQUAL; constant_value = expr; SEMI_COLON { {constant_type; constant_name; constant_value} }

definitions:
    | return_type = typ; function_name = ID; LEFT_PARENTHESIS; parameters = params; RIGHT_PARENTHESIS; LEFT_CURLY_BRACKET;
            statements = list(instruction); RIGHT_CURLY_BRACKET; next_definitions = definitions
        { FunctionDefinition {return_type; function_name; parameters; statements} :: next_definitions }
    | EOF { [] }

do_while_statement:
    | DO; LEFT_CURLY_BRACKET; do_while_statements = list(instruction); RIGHT_CURLY_BRACKET;
        WHILE; LEFT_PARENTHESIS; do_while_condition = expr; RIGHT_PARENTHESIS; SEMI_COLON
        { { do_while_condition; do_while_statements } }

expr:
    | indirection_name = ID; LEFT_SQUARE_BRACKET; indirection_index = expr; RIGHT_SQUARE_BRACKET
        { Indirection { indirection_name; indirection_index } }
    | called_function_name = ID; LEFT_PARENTHESIS; arguments = arguments; RIGHT_PARENTHESIS
        { FunctionCall {called_function_name; arguments} }
    | name = ID { Variable name }
    | character = CHARACTER { Character character }
    | integer = INT { Int integer }
    | conditional_expression = condition { conditional_expression }
    | operation = oper { Operation operation }
    | assignment_operation = assign_oper { AssignmentOperation assignment_operation }
    | string_literal = STRING { String string_literal }
    | variable_name = ID; EQUAL; variable_value = expr { Assignment {variable_name; variable_value} }
    | variable_name = ID; PLUS_PLUS { Increment variable_name }
    | variable_name = ID; MINUS_MINUS { Decrement variable_name }

for_statement:
    | FOR; LEFT_PARENTHESIS; for_init = for_initialization; SEMI_COLON
            ; for_condition = expr; SEMI_COLON
            ; for_increment = expr; RIGHT_PARENTHESIS; LEFT_CURLY_BRACKET;
        for_statements = list(instruction); RIGHT_CURLY_BRACKET
        { { for_init; for_condition; for_increment; for_statements } }

for_initialization:
    | for_init = expr { ForExpression for_init }
    | variable = variable_type_name; EQUAL; variable_value = expr { ForVariableDeclaration {variable with variable_value = Some variable_value} }

if_statement:
    | IF; LEFT_PARENTHESIS; if_condition = expr; RIGHT_PARENTHESIS; LEFT_CURLY_BRACKET;
            if_statements = list(instruction); RIGHT_CURLY_BRACKET
        { { if_condition; if_statements; else_statements = None } }

if_else_statement:
    | statement = if_statement; ELSE; LEFT_CURLY_BRACKET; else_statements = list(instruction); RIGHT_CURLY_BRACKET
        { { statement with else_statements = Some else_statements } }

instruction:
    | BREAK; SEMI_COLON { Break }
    | statement = constant_statement { ConstantDeclaration statement }
    | statement = do_while_statement { DoWhile statement }
    | statement = for_statement { For statement }
    | statement = if_else_statement { If statement }
    | statement = if_statement { If statement }
    | statement = return_statement { Return statement }
    | statement = switch_statement { Switch statement }
    | statement = variable_statement { VariableDeclaration statement }
    | statement = while_statement { While statement }
    | expression = expr; SEMI_COLON { Expression expression }

oper:
    | expression1 = expr; PLUS; expression2 = expr
        { Addition (expression1, expression2) }
    | expression1 = expr; MINUS; expression2 = expr
        { Subtraction (expression1, expression2) }
    (*| expression1 = expr; STAR; expression2 = expr
        { Multiplication (expression1, expression2) }*)
    | expression1 = expr; SLASH; expression2 = expr
        { Division (expression1, expression2) }
    | expression1 = expr; PERCENT; expression2 = expr
        { Modulo (expression1, expression2) }

param:
    | parameter_type = typ; parameter_name = ID { {parameter_type; parameter_name} }
    | parameter_type = typ; parameter_name = ID; LEFT_SQUARE_BRACKET; RIGHT_SQUARE_BRACKET { {parameter_type; parameter_name} }

params:
    parameters = separated_list(COMMA, param) { parameters }

prog:
    | i = definitions { i }

return_statement:
    | RETURN; expression = expr; SEMI_COLON { expression }

switch_statement:
    | SWITCH; LEFT_PARENTHESIS; switch_expression = expr; RIGHT_PARENTHESIS; LEFT_CURLY_BRACKET;
        switch_conditions = list(case); RIGHT_CURLY_BRACKET
        { { switch_expression; switch_conditions } }

typ:
    | current_type = typ; STAR { Pointer current_type }
    | base_type = ID { Type base_type }

variable_statement:
    | variable = variable_type_name; EQUAL; variable_value = expr; SEMI_COLON { {variable with variable_value = Some variable_value} }
    | variable = variable_type_name; SEMI_COLON { variable }

variable_type_name:
    | variable_type = typ; variable_name = ID { {variable_type; variable_name; variable_value = None} }

while_statement:
    | WHILE; LEFT_PARENTHESIS; while_condition = expr; RIGHT_PARENTHESIS; LEFT_CURLY_BRACKET;
        while_statements = list(instruction); RIGHT_CURLY_BRACKET
        { { while_condition; while_statements } }
