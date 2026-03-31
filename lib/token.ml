type t =
  (* Anything else *)
  | Illegal
  (* Identifiers + Literals *)
  | Ident of string
  | Int of string
  (* Operators *)
  | Assign
  | Plus
  (* Delimiters *)
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  (* Keywords *)
  | Function
  | Let
  [@@deriving show { with_path = false }]

let lookup_ident i =
  match i with "fn" -> Function | "let" -> Let | i -> Ident i

let show t = show t
