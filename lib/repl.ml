let start in_c out_c =
  let () = Printf.fprintf out_c ">>> %!" in
  let input = input_line in_c in
  let lexer = Lexer.init input in
  let rec parse lex =
    let lex, tok = Lexer.next_token lex in
    let () = Printf.fprintf out_c "%s\n" (Token.show tok) in
    if tok = Token.Eof then () else parse lex
  in
  parse lexer
