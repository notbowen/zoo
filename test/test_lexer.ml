open Zoo

let ( let* ) = Option.bind

let input =
  {|
  let five = 5;
  let ten = 10;

  let add = fn(x, y) {
    x + y;
  };

  let result = add(five, ten);
  !-/*5;
  5 < 10 > 5;

  if (5 < 10) {
    return true;
  } else {
    return false;
  }

  10 == 10;
  10 != 9;
|}

let%expect_test "lexer" =
  let lexer = Lexer.init input in
  let rec parse lex =
    let lex, tok = Lexer.next_token lex in
    let () = Printf.printf "%s\n" (Token.show tok) in
    if tok = Token.Eof then () else parse lex
  in
  parse lexer;
  [%expect
    {|
    Let
    (Ident "five")
    Assign
    (Int "5")
    Semicolon
    Let
    (Ident "ten")
    Assign
    (Int "10")
    Semicolon
    Let
    (Ident "add")
    Assign
    Function
    LParen
    (Ident "x")
    Comma
    (Ident "y")
    RParen
    LBrace
    (Ident "x")
    Plus
    (Ident "y")
    Semicolon
    RBrace
    Semicolon
    Let
    (Ident "result")
    Assign
    (Ident "add")
    LParen
    (Ident "five")
    Comma
    (Ident "ten")
    RParen
    Semicolon
    Bang
    Minus
    Slash
    Asterisk
    (Int "5")
    Semicolon
    (Int "5")
    Lt
    (Int "10")
    Gt
    (Int "5")
    Semicolon
    If
    LParen
    (Int "5")
    Lt
    (Int "10")
    RParen
    LBrace
    Return
    True
    Semicolon
    RBrace
    Else
    LBrace
    Return
    False
    Semicolon
    RBrace
    (Int "10")
    Eq
    Assign
    (Int "10")
    Semicolon
    (Int "10")
    Neq
    Assign
    (Int "9")
    Semicolon
    Eof
    |}]
