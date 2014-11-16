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
 * TODO: Create helper functions like ends_with to help creating list of things.
 *)

type t = {
    mutable current_token: Lexer.token_with_position;
    parser_lexer: Lexer.t;
}

let advance parsr =
    let { parser_lexer } = parsr in
    parsr.current_token <- Lexer.next_token parser_lexer

let eat parsr token =
    let (tok, _) = parsr.current_token in
    if tok = token then (
        advance parsr;
        true;
    )
    else
        false

let list_of parsr element =
    let rec lst () =
        try
            let el = element parsr in
            let next_elements = lst () in
            el :: next_elements
        with _ -> []
    in lst ()

let separated_by parsr element separator =
    let rec lst () =
        try
            let el = element parsr in
            if eat parsr separator then
                let next_elements = lst () in
                el :: next_elements
            else
                [el]
        with _ -> []
    in lst ()

let rec arguments parsr =
    separated_by parsr expression Lexer.Comma

and expression parsr =
    match parsr.current_token with
    | (Character character, _) ->
            advance parsr;
            Ast.Character character
    | (Identifier called_function_name, _) ->
            advance parsr;
            eat parsr LeftParenthesis;
            let arguments = arguments parsr in
            eat parsr RightParenthesis;
            eat parsr SemiColon;
            Ast.FunctionCall {called_function_name; arguments}
    | (Int integer, _) ->
            advance parsr;
            Ast.Int integer
    | (String string_literal, _) ->
            advance parsr;
            Ast.String string_literal

let identifier parsr =
    match parsr.current_token with
    | (Identifier identifier, _) ->
            advance parsr;
            identifier

let rec array_type parsr typ =
    if eat parsr LeftSquareBracket then (
        advance parsr;
        array_type parsr (Ast.Pointer typ);
    )
    else
        typ

let rec pointer_type parsr typ =
    if eat parsr Star then
        pointer_type parsr (Ast.Pointer typ)
    else
       typ

let typ parsr =
    match parsr.current_token with
    | (Identifier typ, _) ->
            advance parsr;
            pointer_type parsr (Ast.Type typ)

let parameter parsr =
    let parameter_type = typ parsr in
    let parameter_name = identifier parsr in
    let parameter_type = array_type parsr parameter_type in
    {
        Ast.parameter_type;
        Ast.parameter_name;
    }

let parameters parsr =
    separated_by parsr parameter Lexer.Comma

let statement parsr =
    match parsr.current_token with
    | (Identifier _, _) ->
            Ast.Expression (expression parsr)
    | (Return, _) ->
            advance parsr;
            let expression = expression parsr in
            eat parsr SemiColon;
            Ast.Return expression

let statements parsr =
    list_of parsr statement

let declaration parsr =
    let typ = typ parsr in
    let identifier = identifier parsr in
    eat parsr LeftParenthesis;
    let parameter_list = parameters parsr in
    eat parsr RightParenthesis;
    eat parsr LeftCurlyBracket;
    let statement_list = statements parsr in
    eat parsr RightCurlyBracket;
    Ast.FunctionDeclaration {
        return_type = typ;
        function_name = identifier;
        parameters = parameter_list;
        statements = statement_list;
    }

let declarations parsr =
    let rec declarations () =
        match parsr.current_token with
        | (Identifier _, _) ->
                let declaration = declaration parsr in
                let next_declarations = declarations () in
                declaration :: next_declarations
        | (Eof, _) -> []
    in declarations ()

let start parser_lexer =
    declarations {
        current_token = Lexer.next_token parser_lexer;
        parser_lexer;
    }

let parse filename =
    let lexer = Lexer.create filename in
    let ast = start lexer in
    Lexer.close lexer;
    ast
