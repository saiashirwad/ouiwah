type value =
  | Number of float
  | Array of value array
  | Function of string * expr list

and expr =
  | Value of value
  | Identifier of string
  | PrimOp of string
  | FunctionDef of string * expr list

type program = expr list
