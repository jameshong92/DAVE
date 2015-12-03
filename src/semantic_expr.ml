(* sub-routines called in shcek_expr in scheck.ml *)
open Ast
open Sast
open Printf

let check_unop unop s_expr =
  let ret = S_Unop(unop, s_expr) in
  match unop with
    Not -> (match s_expr with
        Bool, _ -> Bool, ret
      | _, _ -> raise (Invalid_use "\"not\" bad operand type"))
  | Neg -> (match s_expr with
        Int, _ -> Int, ret
      | Float, _ -> Float, ret
      | _, _ -> raise (Invalid_use "unary \"-\" bad operand type"))

let check_postop s_expr postop =
  let ret = S_Postop(s_expr postop) in
  match postop with
    Inc -> (match s_expr with
        Int, _ -> Int, ret
      | _, _ -> raise (Invalid_use "\"++\" bad operand type"))
  | Dec -> (match s_expr with
        Int, _ -> Int, ret
      | _, _ -> raise (Invalid_use "\"--\" bad operand type"))

let check_binop s_expr1 binop s_expr2 =
  let t1, _ = s_expr1 in
  let t2, _ = s_expr2 in
  let ret0 = S_Binop(s_expr1, binop, s_expr2) in
  match binop with
    Add -> (match t1, t2 with
      (* scalar arithmetic binary op *)
        Int, Int -> Int, ret0
      | Float, Float -> Float, ret0
      | Int, Float -> Float, ret0
      | Float, Int -> Float, ret0
      | String, String -> String, ret0
      | _, _ -> raise (Invalid_use "\"+\" bad operand type"))
  | Sub -> (match t1, t2 with
      (* scalar arithmetic binary op *)
        Int, Int -> Int, ret0
      | Float, Float -> Float, ret0
      | Int, Float -> Float, ret0
      | Float, Int -> Float, ret0
      | _, _ -> raise (Invalid_use "\"-\" bad operand type"))
  | Mul -> (match t1, t2 with
      (* scalar arithmetic binary op *)
        Int, Int -> Int, ret0
      | Float, Float -> Float, ret0
      | Int, Float -> Float, ret0
      | Float, Int -> Float, ret0
      | _, _ -> raise (Invalid_use "\"*\" bad operand type"))
  | Div -> (match t1, t2 with
      (* scalar arithmetic binary op *)
        Int, Int -> Int, ret0
      | Float, Float -> Float, ret0
      | Int, Float -> Float, ret0
      | Float, Int -> Float, ret0
      | _, _ -> raise (Invalid_use "\"/\" bad operand type"))
  | Mod -> (match t1, t2 with
      (* scalar arithmetic binary op *)
        Int, Int -> Int, ret0
      | Float, Float -> Float, ret0
      | Int, Float -> Float, ret0
      | Float, Int -> Float, ret0
      | _, _ -> raise (Invalid_use "\"/\" bad operand type"))
  | Exp -> (match t1, t2 with
      (* scalar arithmetic binary op *)
        Int, Int -> Int, ret0
      | Float, Float -> Float, ret0
      | Int, Float -> Float, ret0
      | Float, Int -> Float, ret0
      | _, _ -> raise (Invalid_use "\"/\" bad operand type"))
  | Eq -> (match t1, t2 with
        Int, Int -> Bool, ret0
      | Float, Float -> Bool, ret0
      | Int, Float -> Bool, ret0
      | Float, Int -> Bool, ret0
      | String, String -> Bool, ret0
      | Bool, Bool -> Bool, ret0
      | _, _ -> raise (Invalid_use "\"=\" bad operand type"))
  | Neq -> (match t1, t2 with
        Int, Int -> Bool, ret0
      | Float, Float -> Bool, ret0
      | Int, Float -> Bool, ret0
      | Float, Int -> Bool, ret0
      | String, String -> Bool, ret0
      | _, _ -> raise (Invalid_use "\"!=\" bad operand type"))
  | Lt -> (match t1, t2 with
        Int, Int -> Bool, ret0
      | Float, Float -> Bool, ret0
      | Int, Float -> Bool, ret0
      | Float, Int -> Bool, ret0
      | String, String -> Bool, ret0
      | _, _ -> raise (Invalid_use "\"<\" bad operand type"))
  | Leq -> (match t1, t2 with
        Int, Int -> Bool, ret0
      | Float, Float -> Bool, ret0
      | Int, Float -> Bool, ret0
      | Float, Int -> Bool, ret0
      | String, String -> Bool, ret0
      | _, _ -> raise (Invalid_use "\"<=\" bad operand type"))
  | Gt -> (match t1, t2 with
        Int, Int -> Bool, ret0
      | Float, Float -> Bool, ret0
      | Int, Float -> Bool, ret0
      | Float, Int -> Bool, ret0
      | String, String -> Bool, ret0
      | _, _ -> raise (Invalid_use "\">\" bad operand type"))
  | Geq -> (match t1, t2 with
        Int, Int -> Bool, ret0
      | Float, Float -> Bool, ret0
      | Int, Float -> Bool, ret0
      | Float, Int -> Bool, ret0
      | String, String -> Bool, ret0
      | _, _ -> raise (Invalid_use "\">=\" bad operand type"))
  | And -> (match t1, t2 with
        Bool, Bool -> Bool, ret0
      | _, _ -> raise (Invalid_use "\"and\" bad operand type"))
  | Or -> (match t1, t2 with
        Bool, Bool -> Bool, ret0
      | _, _ -> raise (Invalid_use "\"or\" bad operand type"))

let check_assign s_expr1 s_expr2 =
  let t1, _ = s_expr1 in
  let t2, _ = s_expr2 in
  let ret0 = S_Assign(s_expr1, s_expr2) in
  match t1, t2 with
  (* scalar assignment *)
    Int, Int -> Int, ret0
  | Float, Int -> Float, ret0
  | Float, Float -> Float, ret0
  | Int, Float -> Float, ret0
  | String, String -> String, ret0
  | Bool, Bool -> Bool, ret0
  | x, y -> raise (Invalid_use (sprintf "%s : %s operand types invalid" (pt x) (pt y)))
