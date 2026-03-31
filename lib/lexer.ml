let ( let* ) = Option.bind

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

let is_letter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'
let is_digit c = '0' <= c && c <= '9'

let read_identifier lexer =
  let rec read_ident lexer pos =
    match lexer with
    | None -> (None, pos)
    | Some lex ->
        if not (is_letter lex.ch) then (lexer, pos)
        else read_ident (advance_char lex) (pos + 1)
  in
  let start = lexer.position in
  let input = lexer.input in
  let next_lexer, stop = read_ident (Some lexer) start in
  let ident = String.sub input start (stop - start) in
  Some (next_lexer, Token.lookup_ident ident)

let read_number lexer =
  let rec read_num lexer pos =
    match lexer with
    | None -> (None, pos)
    | Some lex ->
        if not (is_digit lex.ch) then (lexer, pos)
        else read_num (advance_char lex) (pos + 1)
  in
  let start = lexer.position in
  let input = lexer.input in
  let next_lexer, stop = read_num (Some lexer) start in
  let num = String.sub input start (stop - start) in
  Some (next_lexer, Token.Int num)

let rec skip_whitespace lexer =
  let* lexer = lexer in
  match lexer.ch with
  | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (advance_char lexer)
  | _ -> Some lexer

let next_token lexer =
  let* lexer = skip_whitespace lexer in
  let open Token in
  if is_letter lexer.ch then read_identifier lexer
  else if is_digit lexer.ch then read_number lexer
  else
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
      | _ -> Illegal
    in
    let lexer = advance_char lexer in
    Some (lexer, token)
