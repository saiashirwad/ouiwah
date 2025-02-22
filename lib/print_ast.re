open Ast;

type user = {
  name: string,
  mutable isAdmin: option(bool),
};

let matchThings =
  switch ("hi") {
  | "what" => "hi"
  | _ => "no"
  };

let print = Printf.sprintf;

let rec ast_value_to_string = (ident: string, value: value): string =>
  switch (value) {
  | Number(n) => Printf.sprintf("%sNumber(%f)\n", ident, n)
  | Array(arr) => string_of_arr(ident, arr)
  | Function(name, body) => string_of_function(ident, name, body)
  }
and string_of_ast_expr = (ident: string, expr: expr): string =>
  switch (expr) {
  | _ => ident
  }
and string_of_arr = (ident, arr) => {
  let array_start = Printf.sprintf("%sArray[\n", ident);
  let array_content =
    Array.fold_left(
      (acc, item) => acc ++ ast_value_to_string(ident ++ "  ", item),
      "",
      arr,
    );
  let arrayEnd = Printf.sprintf("%s]\n", ident);
  array_start ++ array_content ++ arrayEnd;
}
and string_of_function = (ident, name, body) => {
  Printf.sprintf(
    "%sFunction(\n%s  name: %s\n%s  body: [\n%s%s  ]\n%s)\n",
    ident,
    ident,
    name,
    ident,
    List.fold_left(
      (acc, expr) => acc ++ string_of_ast_expr(ident ++ "    ", expr),
      "",
      body,
    ),
    ident,
    ident,
  );
};
