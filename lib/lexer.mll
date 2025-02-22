{
open Parser

exception LexError of string
}

let whitespace = [' ' '\t' '\r' '\n']
let digit = ['0'-'9']
let number = '-'? digit+ ('.' digit*)?
let identifier = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | whitespace    { token lexbuf }
  | '#'           { comment lexbuf }
  | number as n   { NUMBER (float_of_string n) }
  | '['          { LBRACKET }
  | ']'          { RBRACKET }
  | "fn"         { FN }
  | '+'          { PLUS }
  | '-'          { MINUS }
  | '*'          { TIMES }
  | '/'          { DIVIDE }
  | "dup"        { DUP }
  | "drop"       { DROP }
  | "swap"       { SWAP }
  | "over"       { OVER }
  | identifier as id { IDENTIFIER id }
  | eof          { EOF }
  | _ as c       { raise (LexError ("Unexpected character: " ^ String.make 1 c)) }

and comment = parse
  | '\n'         { token lexbuf }
  | eof          { EOF }
  | _           { comment lexbuf }
