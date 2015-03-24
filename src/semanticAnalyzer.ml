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

let check_list (check_function: 'a -> bool) (list: 'a list): bool =
    let rec check result = function
        | [] -> result
        | x :: xs ->
                let new_result = check_function x in
                check (result && new_result) xs
    in check true list

let ty_of_expression = function
    | Ast.Int _ -> Int
    | Ast.String _ -> Pointer Char

let check_lists_match check_function list1 list2 =
    let rec check result list1 list2 = match (list1, list2) with
        | ([], []) -> result
        (* check count mismatch *)
        | (x :: xs, y :: ys) ->
                let new_result = check_function x y in
                check (result && new_result) xs ys
    in check true list1 list2

let ty_of_parameter { Ast.parameter_type } = ty_of_typ parameter_type

class analyzer (environment: Environment.environment) =
    object (self)
        val mutable errors = ([| |] : Lexer.error_message array)

        method private add_error error =
            errors <- Array.append errors [| error |]

        method private add_parameter {Ast.parameter_type; Ast.parameter_name} =
            environment#add_declaration parameter_name (VariableDec (ty_of_typ parameter_type));
            true

        method private check_expression = function
            | Ast.FunctionCall {Ast.called_function_name; Ast.arguments; Ast.file_position} ->
                    (match environment#look_declaration called_function_name with
                    | Some (FunctionDec {Environment.parameters}) ->
                            check_lists_match self#expression_matches_type parameters arguments
                            (* TODO: check argument type. *)
                    | Some (VariableDec _) -> true (* TODO: cannot call a variable. *)
                    | None ->  self#add_error
                               { Lexer.error_message = "Undefined function `" ^ called_function_name ^ "`"
                               ; Lexer.error_position = file_position};
                               false
                    )
            | _ -> true (* TODO: check other expression *)

        method private check_function (Ast.FunctionDeclaration { Ast.statements }) =
            check_list self#check_statement statements

        method private check_global_declaration = function
            | Ast.FunctionDeclaration { Ast.function_name; Ast.parameters; Ast.return_type } as fnctn ->
                    (match environment#look_declaration function_name with
                    | Some declaration -> () (* TODO: check type with existing, and check that it is not the second declaration *)
                    | None -> environment#add_declaration function_name (FunctionDec
                                { parameters = List.map ty_of_parameter parameters
                                ; result = ty_of_typ return_type
                                })
                    );
                    environment#enter_scope;
                    check_list self#add_parameter parameters;
                    let valid = self#check_function fnctn in
                    environment#leave_scope;
                    valid
            (*| FunctionPrototype { function_name } as fnctn ->*)
                    (*add_function function_name fnctn*)
                    
        method check_global_declarations = function
            | Ast.File declarations ->
                    check_list self#check_global_declaration declarations
            | Ast.NoFile -> false

        method private check_statement = function
            | Ast.Expression expression -> self#check_expression expression
            | Ast.Return expression -> true (* TODO: check that the return value has the right type *)

        method private expression_matches_type ty expression =
            if ty = ty_of_expression expression
                then true
                else (
                    self#add_error
                    { Lexer.error_message = "expected `" ^ string_of_ty ty ^ "`, but argument is of type `" ^ string_of_ty (ty_of_expression expression) ^ "`"
                    (* TODO: add real position *)
                    ; Lexer.error_position = {
                        position_column = 1;
                        position_filename = "";
                        position_line = 1;
                    }};
                    false
                )

        method errors = errors
    end

let analyze ast =
    let environment = new Environment.environment in
    let analyzer = new analyzer environment in
    if analyzer#check_global_declarations ast
        then ast
        else
            let errors = analyzer#errors in
            Array.iter (fun error ->
                let {Lexer.error_message; Lexer.error_position} = error in
                Utils.print_error error_message error_position
            ) errors;
            Ast.NoFile
