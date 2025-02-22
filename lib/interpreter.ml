open Ast

type stack = value list
type environment = (string * value) list
type interpreter_state = { stack : stack; env : environment }

exception Runtime_error of string

let push v state = { state with stack = v :: state.stack }

let pop state =
  match state.stack with
  | [] -> raise (Runtime_error "Stack underflow")
  | v :: rest -> (v, { state with stack = rest })

let pop2 state =
  let v1, state1 = pop state in
  let v2, state2 = pop state1 in
  (v2, v1, state2)

(* Stack operations *)
let apply_stack_op op state =
  match op with
  | "dup" ->
      let v, _ = pop state in
      push v (push v state)
  | "drop" ->
      let _, state' = pop state in
      state'
  | "swap" ->
      let v1, state1 = pop state in
      let v2, state2 = pop state1 in
      push v1 (push v2 state2)
  | "over" ->
      let v1, state1 = pop state in
      let v2, state2 = pop state1 in
      push v2 (push v1 (push v2 state2))
  | _ -> raise (Runtime_error ("Unknown stack operation: " ^ op))

let apply_binop op state =
  let v1, v2, state' = pop2 state in
  match (v1, v2, op) with
  | Number n1, Number n2, "+" -> push (Number (n1 +. n2)) state'
  | Number n1, Number n2, "-" -> push (Number (n1 -. n2)) state'
  | Number n1, Number n2, "*" -> push (Number (n1 *. n2)) state'
  | Number n1, Number n2, "/" ->
      if n2 = 0.0 then raise (Runtime_error "Division by zero")
      else push (Number (n1 /. n2)) state'
  | _ -> raise (Runtime_error ("Invalid operands for " ^ op))

(* Function evaluation *)
let rec eval_function state fn_body = List.fold_left eval_expr state fn_body

and eval_expr state = function
  | Value v -> push v state
  | PrimOp op -> (
      match op with
      | "+" | "-" | "*" | "/" -> apply_binop op state
      | "dup" | "drop" | "swap" | "over" -> apply_stack_op op state
      | _ -> raise (Runtime_error ("Unknown operator: " ^ op)))
  | Identifier id -> (
      match List.assoc_opt id state.env with
      | Some (Function (_, body)) -> eval_function state body
      | Some v -> push v state
      | None -> raise (Runtime_error ("Undefined identifier: " ^ id)))
  | FunctionDef (name, body) ->
      let fn = Function (name, body) in
      { state with env = (name, fn) :: state.env }

let eval program =
  let initial_state = { stack = []; env = [] } in
  let final_state = List.fold_left eval_expr initial_state program in
  final_state.stack
