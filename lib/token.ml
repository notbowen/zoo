type t =
  (* Anything else *)
  | Illegal
  | Eof
  (* Identifiers + Literals *)
  | Ident
  | Int
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
[@@deriving show]

let show t = show t
