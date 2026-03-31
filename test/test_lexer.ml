open Zoo

let input = "=+(){},;"

let%expect_test "lexer" =
  let lexer = Lexer.init input in
  let rec parse lex =
    match lex with
    | Some lex ->
        let lex, tok = Lexer.next_token lex in
        let () = Printf.printf "%s\n" (Token.show tok) in
        parse lex
    | None -> ()
  in
  parse lexer;
  [%expect {|
    Token.Assign
    Token.Plus
    Token.LParen
    Token.RParen
    Token.LBrace
    Token.RBrace
    Token.Comma
    Token.Semicolon
    |}]
