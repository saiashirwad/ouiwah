%{
open Ast
%}

%token <float> NUMBER
%token <string> IDENTIFIER
%token LBRACKET RBRACKET FN
%token PLUS MINUS TIMES DIVIDE
%token EOF
%token DUP DROP SWAP OVER

%start <program> program

%%

program:
  | defns = list(function_def) exprs = list(expr) EOF { defns @ exprs }

function_def:
  | id = IDENTIFIER FN body = nonempty_list(simple_expr) { FunctionDef (id, body) }

simple_expr:
  | n = NUMBER { Value (Number n) }
  | id = IDENTIFIER { Identifier id }
  | PLUS { PrimOp "+" }
  | MINUS { PrimOp "-" }
  | TIMES { PrimOp "*" }
  | DIVIDE { PrimOp "/" }
  | DUP { PrimOp "dup" }
  | DROP { PrimOp "drop" }
  | SWAP { PrimOp "swap" }
  | OVER { PrimOp "over" }

expr:
  | e = simple_expr { e }
  | LBRACKET vs = list(expr) RBRACKET { 
      let values = List.map (function
        | Value v -> v
        | _ -> failwith "Array can only contain values"
      ) vs in
      Value (Array (Array.of_list values))
    } 