type token =
    | Break
    | Case
    | Character of char
    | Colon
    | Comma
    | Const
    | Default
    | Divide
    | DivideEqual
    | Do
    | Else
    | Eof
    | Equal
    | Float of float
    | For
    | Greater
    | GreaterOrEqual
    | Identifier of string
    | If
    | Int of int
    | IsEqual
    | LeftCurlyBracket
    | LeftParenthesis
    | LeftSquareBracket
    | Lesser
    | LesserOrEqual
    | Minus
    | MinusEqual
    | Modulo
    | ModuloEqual
    | Not
    | NotEqual
    | Plus
    | PlusEqual
    | Return
    | RightCurlyBracket
    | RightParenthesis
    | RightSquareBracket
    | SemiColon
    | String of string
    | Switch
    | Times
    | TimesEqual
    | While

type file_position = int * int

type token_with_position = token * file_position

exception UnexpectedCharacter of char * file_position

val close : unit -> unit

val tokens : string -> token_with_position Stream.t
