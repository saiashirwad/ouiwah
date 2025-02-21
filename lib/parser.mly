%{
open Ast
%}

%token <float> NUMBER
%token <string> IDENTIFIER
%token LBRACKET RBRACKET FN
%token PLUS MINUS TIMES DIVIDE
%token EOF

%start <program> program

%%

program:
  | exprs = list(expr) EOF { exprs }

expr:
  | n = NUMBER { Value (Number n) }
  | id = IDENTIFIER { Identifier id }
  | LBRACKET vs = list(expr) RBRACKET { 
      let values = List.map (function
        | Value v -> v
        | _ -> failwith "Array can only contain values"
      ) vs in
      Value (Array (Array.of_list values))
    }
  | id = IDENTIFIER FN body = list(expr) { FunctionDef (id, body) }
  | PLUS { PrimOp "+" }
  | MINUS { PrimOp "-" }
  | TIMES { PrimOp "*" }
  | DIVIDE { PrimOp "/" }
  ; 