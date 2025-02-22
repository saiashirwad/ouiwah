type value =
  | Number(float)
  | Array(array(value))
  | Function(string, list(expr))

and expr =
  | Value(value)
  | Identifier(string)
  | PrimOp(string)
  | FunctionDef(string, list(expr));

type program = list(expr);
