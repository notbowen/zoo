type t = { input : string; position : int; ch : char }

let init input =
  if String.length input = 0 then None
  else Some { input; position = 0; ch = input.[0] }

let advance_char lexer =
  let length = String.length lexer.input in
  let position = lexer.position + 1 in
  if position >= length then None
  else
    let ch = String.get lexer.input position in
    Some { input = lexer.input; position; ch }

let next_token lexer =
  let open Token in
  let token =
    match lexer.ch with
    | '=' -> Assign
    | ';' -> Semicolon
    | '(' -> LParen
    | ')' -> RParen
    | ',' -> Comma
    | '+' -> Plus
    | '{' -> LBrace
    | '}' -> RBrace
    | _ -> Eof
  in
  let lexer = advance_char lexer in
  (lexer, token)
