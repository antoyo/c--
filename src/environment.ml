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

type ty = Array of ty
        | Char
        | Int
        | Float
        | Pointer of ty
        | Structure of (string * ty) list
        | Variadic of ty
        | Void

module StringMap = Map.Make (String)

type 'a symbol_table = 'a StringMap.t Stack.t

type function_dec = { parameters: ty list
                    ; result: ty
                    }

type declaration = VariableDec of ty
                 | FunctionDec of function_dec

class environment =
    object (self)
        val types = Stack.create ()
        val declarations = Stack.create ()
        
        initializer
            Stack.push StringMap.empty types;
            Stack.push StringMap.empty declarations;
            List.iter (Utils.curry self#add_type) [("int", Int); ("char", Char); ("float", Float)];
            List.iter (Utils.curry self#add_declaration)
                [ ("puts", FunctionDec {parameters = [Pointer Char]; result = Void})
                ; ("printf", FunctionDec {parameters = [Pointer Char; Variadic Void]; result = Void})
            ]

        method add_declaration key value =
            let map = Stack.pop declarations in
            let new_map = StringMap.add key value map in
            Stack.push new_map declarations

        method add_type key value =
            let map = Stack.pop types in
            let new_map = StringMap.add key value map in
            Stack.push new_map types

        method enter_scope =
            let stack = declarations in
            let map = Stack.top stack in
            Stack.push map stack

        method leave_scope =
            let stack = declarations in
            let _ = Stack.pop stack in
            ()

        method look_declaration key =
            let map = Stack.top declarations in
            if StringMap.mem key map
                then Some (StringMap.find key map)
                else None

        method look_type key =
            let map = Stack.top types in
            if StringMap.mem key map
                then Some (StringMap.find key map)
                else None
    end

let rec string_of_ty = function
    | Char -> "char"
    | Int -> "int"
    | Pointer ty -> string_of_ty ty ^ "*"
