type t = { input : string; position : int; ch : char }

let init input =
  let ch = if String.length input = 0 then '\x00' else input.[0] in
  { input; position = 0; ch }

let advance_char lexer =
  let length = String.length lexer.input in
  let position = lexer.position + 1 in
  let ch =
    if position >= length then '\x00' else String.get lexer.input position
  in
  { input = lexer.input; position; ch }

let peek_char lexer =
  let length = String.length lexer.input in
  let peek_pos = lexer.position + 1 in
  if peek_pos >= length then None else Some lexer.input.[peek_pos]

let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'
let is_digit c = '0' <= c && c <= '9'

let read_identifier lexer =
  let rec read_ident lexer pos =
    if not (is_letter lexer.ch) then (lexer, pos)
    else read_ident (advance_char lexer) (pos + 1)
  in
  let start = lexer.position in
  let input = lexer.input in
  let next_lexer, stop = read_ident lexer start in
  let ident = String.sub input start (stop - start) in
  (next_lexer, Token.lookup_ident ident)

let read_number lexer =
  let rec read_num lexer pos =
    if not (is_digit lexer.ch) then (lexer, pos)
    else read_num (advance_char lexer) (pos + 1)
  in
  let start = lexer.position in
  let input = lexer.input in
  let next_lexer, stop = read_num lexer start in
  let num = String.sub input start (stop - start) in
  (next_lexer, Token.Int num)

let rec skip_whitespace lexer =
  match lexer.ch with
  | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (advance_char lexer)
  | _ -> lexer

let next_token lexer =
  let lexer = skip_whitespace lexer in
  let open Token in
  if is_letter lexer.ch then read_identifier lexer
  else if is_digit lexer.ch then read_number lexer
  else
    let token =
      match lexer.ch with
      | '=' when peek_char lexer = Some '=' -> Eq
      | '=' -> Assign
      | '+' -> Plus
      | '-' -> Minus
      | '!' when peek_char lexer = Some '=' -> Neq
      | '!' -> Bang
      | '/' -> Slash
      | '*' -> Asterisk
      | '<' -> Lt
      | '>' -> Gt
      | ';' -> Semicolon
      | '(' -> LParen
      | ')' -> RParen
      | ',' -> Comma
      | '{' -> LBrace
      | '}' -> RBrace
      | '\x00' -> Eof
      | _ -> Illegal
    in
    let lexer = advance_char lexer in
    (lexer, token)
