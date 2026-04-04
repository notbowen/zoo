type t = { lexer : Lexer.t; current : Token.t; peek : Token.t }

let next_token parser =
  let lexer = parser.lexer in
  let current = parser.peek in
  let lexer, peek = Lexer.next_token lexer in
  { lexer; current; peek }

let init lexer =
  let parser = { lexer; current = Token.Eof; peek = Token.Eof } in
  let parser = next_token parser in
  next_token parser

let rec skip_to_semicolon parser =
  match parser.current with
  | Semicolon -> parser
  | _ -> skip_to_semicolon (next_token parser)

let parse_identifier parser =
  match parser.peek with
  | Ident i -> (next_token parser, i)
  | t ->
      failwith
        (Printf.sprintf "expected identifier, got %s instead" (Token.show t))

let parse_let_statement parser =
  let parser, name = parse_identifier parser in
  let parser = skip_to_semicolon parser in
  (parser, Ast.Let { name; value = "" })

let parse_return_statement parser =
  let parser = skip_to_semicolon parser in
  (parser, Ast.Return { value = "" })

let parse_statement parser =
  match parser.current with
  | Let -> parse_let_statement parser
  | Return -> parse_return_statement parser
  | t -> failwith (Printf.sprintf "%s not implemented yet" (Token.show t))

let parse_program parser =
  let rec parse parser stmts =
    if parser.current = Token.Eof then stmts
    else
      let parser, stmt = parse_statement parser in
      let parser = next_token parser in
      parse parser (stmt :: stmts)
  in
  List.rev (parse parser [])
