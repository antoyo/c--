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

let read_size = 4096

let buffer1 = Buffer.create (read_size + 1)

let buffer2 = Buffer.create (read_size + 1)

let bytes = Bytes.create read_size

let channel = ref stdin

let column = ref 0

let line = ref 0

let close_file () =
    close_in !channel

let current_buffer = ref buffer1
let current_pointer = ref 0

let file_position () = (!line, !column)

let read buffer start len =
    let length = input !channel bytes start len in
    Bytes.set bytes length eof;
    Buffer.add_bytes buffer bytes

let get_char () =
    let character = Buffer.nth !current_buffer !current_pointer in
    if character = eof
        then raise End_of_file
        else character

let get_current_char () =
    Buffer.nth !current_buffer !current_pointer

let get_next_char () =
    Buffer.nth !current_buffer (!current_pointer + 1)

let next_char () =
    incr current_pointer;
    if get_current_char () = '\n' || (get_current_char () = '\r' && get_next_char () <> '\n') then (
        incr line;
        column := 0;
    )
    else
        incr column

let open_file filename =
    channel := open_in filename;
    read buffer1 0 read_size;
    try
        read buffer2 read_size read_size
    with _ ->()

let previous_char () =
    decr current_pointer

let start_buffer = ref buffer1
let start_pointer = ref 0

let adjust_start_position () =
    start_buffer := !start_buffer;
    start_pointer := !current_pointer

let substring () =
    Buffer.sub !current_buffer !start_pointer (!current_pointer - !start_pointer + 1)
