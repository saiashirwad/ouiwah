open Ast
open Printf

let print = printf

let rec print_ast_value indent = function
  | Number n -> print "%sNumber(%f)\n" indent n
  | Array arr ->
      print "%sArray[\n" indent;
      Array.iter (fun v -> print_ast_value (indent ^ "  ") v) arr;
      print "%s]\n" indent
  | Function (name, body) ->
      print "%sFunction(\n" indent;
      print "%s  name: %s\n" indent name;
      print "%s  body: [\n" indent;
      List.iter (fun expr -> print_ast_expr (indent ^ "    ") expr) body;
      print "%s  ]\n" indent;
      print "%s)\n" indent

and print_ast_expr indent = function
  | Value v ->
      print "%sValue(\n" indent;
      print_ast_value (indent ^ "  ") v;
      print "%s)\n" indent
  | Identifier id -> print "%sIdentifier(%s)\n" indent id
  | PrimOp op -> print "%sPrimOp(%s)\n" indent op
  | FunctionDef (name, body) ->
      print "%sFunctionDef(\n" indent;
      print "%s  name: %s\n" indent name;
      print "%s  body: [\n" indent;
      List.iter (fun expr -> print_ast_expr (indent ^ "    ") expr) body;
      print "%s  ]\n" indent;
      print "%s)\n" indent

let print_ast ast =
  print "Program[\n";
  List.iter (print_ast_expr "  ") ast;
  print "]\n"
