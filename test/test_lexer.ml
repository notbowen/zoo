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
|}

let%expect_test "lexer" =
  let lexer = Lexer.init input in
  let rec parse lex =
    let res = Lexer.next_token lex in
    match res with
    | None -> ()
    | Some (lex, tok) ->
        let () = Printf.printf "%s\n" (Token.show tok) in
        parse lex
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
    |}]
