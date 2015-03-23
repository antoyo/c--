(*
 * Copyright (C) 2015  Boucher, Antoni <bouanto@gmail.com>
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
 * Impérative :
 * L’environemment est un Hashtbl contenant des Stack.
 * Quand une variable est ajoutée, elle est mise sur la pile.
 * Quand on sort de la portée de la variable, elle est enlevée du dessus de la pile.
 *
 * Écrire un vérificateur de type qui affiche un message d’erreur lorsque les types ne correspondent pas ou lorsqu’il y a des identifiants non déclarés (modules Environment et SemanticAnalyzer).
 * Produire un AST typé (GADT).
 * Vérification que break/continue sont dans des boucles/switch.
 *)

open Environment

exception SemanticError of Lexer.error_message

let ty_of_typ = function
    | Ast.Type "char" -> Char
    | Ast.Type "int" -> Int

let ty_of_parameter { Ast.parameter_type } = ty_of_typ parameter_type

class analyzer (environment: Environment.environment) =
    object (self)
        method private add_parameter {Ast.parameter_type; Ast.parameter_name} =
            environment#add_declaration parameter_name (VariableDec (ty_of_typ parameter_type))

        method private check_expression = function
            | Ast.FunctionCall {Ast.called_function_name; Ast.arguments} ->
                    (match environment#look_declaration called_function_name with
                    | Some _ -> ()
                    | None -> raise (SemanticError { Lexer.error_message = "Undefined function `" ^ called_function_name ^ "`"
                                                   (* TODO: put real error position *)
                                                   ; Lexer.error_position = {
                                                       position_column = 0;
                                                       position_filename = "";
                                                       position_line = 0;
                                                   }})
                    )
                    (* TODO: check argument type and return type. *)
            | _ -> () (* TODO: check other expression *)

        method private check_function (Ast.FunctionDeclaration { Ast.statements }) =
            List.iter self#check_statement statements

        method check_global_declaration : Ast.declaration -> unit = function
            | Ast.FunctionDeclaration { Ast.function_name; Ast.parameters; Ast.return_type } as fnctn ->
                    (match environment#look_declaration function_name with
                    | Some declaration -> () (* TODO: check type with existing, and check that it is not the second declaration *)
                    | None -> environment#add_declaration function_name (FunctionDec
                                { parameters = List.map ty_of_parameter parameters
                                ; result = ty_of_typ return_type
                                })
                    );
                    environment#enter_scope;
                    List.iter self#add_parameter parameters;
                    self#check_function fnctn;
                    environment#leave_scope
            (*| FunctionPrototype { function_name } as fnctn ->*)
                    (*add_function function_name fnctn*)

        method private check_statement = function
            | Ast.Expression expression -> self#check_expression expression
            | Ast.Return expression -> () (* TODO: check that the return value has the right type *)
    end

let analyze ast =
    let environment = new Environment.environment in
    let analyzer = new analyzer environment in
    try
        List.iter analyzer#check_global_declaration ast;
        ast
    with SemanticError semantic_error ->
        let {Lexer.error_message; Lexer.error_position} = semantic_error in
        Utils.print_error error_message error_position;
        []
