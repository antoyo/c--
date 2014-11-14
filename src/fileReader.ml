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

let eof = char_of_int 4

let stream = ref (Stream.of_list [] : char Stream.t)

let channel = ref stdin

let column = ref 1

let line = ref 1

let close_file () =
    close_in !channel

let file_position () = (!line, !column)

let get_current_char () =
    match Stream.peek !stream with
    | Some character -> character
    | None -> eof

let get_char () =
    match Stream.peek !stream with
    | Some character -> character
    | None -> raise End_of_file

let get_next_char_noerr () =
    match Stream.npeek 2 !stream with
    | [_; character] -> character
    | _ -> eof

let get_next_char () =
    match Stream.npeek 2 !stream with
    | [_; character] -> character
    | _ -> raise End_of_file

let next_char () =
    Stream.junk !stream;
    if get_current_char () = '\n' || (get_current_char () = '\r' && get_next_char_noerr () <> '\n') then (
        incr line;
        column := 0;
    )
    else
        incr column

let open_file filename =
    channel := open_in filename;
    stream := Stream.of_channel !channel
