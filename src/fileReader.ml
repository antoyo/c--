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

type t = {
    file_channel: in_channel;
    mutable file_column: int;
    mutable file_line: int;
    file_name: string;
    file_stream: char Stream.t;
}

type file_position = {
    position_column: int;
    position_filename: string;
    position_line: int;
}

let eof = char_of_int 4

let close_file reader =
    close_in reader.file_channel

let file_position reader = {
    position_column = reader.file_column;
    position_filename = reader.file_name;
    position_line = reader.file_line;
}

let get_current_char reader =
    match Stream.peek reader.file_stream with
    | Some character -> character
    | None -> eof

let get_char reader =
    match Stream.peek reader.file_stream with
    | Some character -> character
    | None -> raise End_of_file

let get_next_char_noerr reader =
    match Stream.npeek 2 reader.file_stream with
    | [_; character] -> character
    | _ -> eof

let get_next_char reader =
    match Stream.npeek 2 reader.file_stream with
    | [_; character] -> character
    | _ -> raise End_of_file

let next_column reader =
    reader.file_column <- reader.file_column + 1

let next_line reader =
    reader.file_column <- 0;
    reader.file_line <- reader.file_line + 1

let next_char reader =
    Stream.junk reader.file_stream;
    if get_current_char reader = '\n' || (get_current_char reader = '\r' && get_next_char_noerr reader <> '\n') then 
        next_line reader
    else
        next_column reader

let open_file filename =
    let channel = open_in filename in
    {
        file_channel = channel;
        file_column = 0;
        file_line = 1;
        file_name = filename;
        file_stream = Stream.of_channel channel;
    }
