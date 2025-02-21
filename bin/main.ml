open Ouiwah

let parse_program input =
  let lexbuf = Lexing.from_string input in
  try Parser.program Lexer.token lexbuf with
  | Lexer.LexError msg ->
      Printf.fprintf stderr "Lexing error: %s\n" msg;
      exit 1
  | Parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.fprintf stderr "Parsing error at line %d, character %d\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      exit 1

let () =
  let examples =
    [
      {|
    # Basic arithmetic
    2 3 +
    4 5 *
    |};
      {|
    # Stack operations (using function definitions)
    2 3
    |};
      {|
    # Function definition and arrays
    double fn dup +
    [1 2 3]
    |};
      {|
    # Array operations
    [1 2 3]
    [4 5 6]
    |};
    ]
  in

  List.iter
    (fun input ->
      print_endline "\nInput:";
      print_endline input;
      print_endline "\nAST:";
      let ast = parse_program input in
      Ast_printer.print_ast ast;
      print_endline "\nResult:";
      let result = Interpreter.eval ast in
      print_endline "Stack:";
      List.iter (fun v -> Ast_printer.print_ast_value "  " v) result;
      print_endline "")
    examples
