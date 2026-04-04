open Zoo

(* Helper functions *)
let create_parser input =
  let lexer = Lexer.init input in
  Parser.init lexer

let print_program parser =
  let stmts = Parser.parse_program parser in
  List.iter (Format.printf "%a\n" Ast.pp_statement) stmts

(* Actual tests *)
let let_input = {|
  let x = 5;
  let y = 10;
  let foobar = 838383;
|}

let%expect_test "test_let" =
  create_parser let_input |> print_program;
  [%expect
    {|
    Let {name = "x"; value = ""}
    Let {name = "y"; value = ""}
    Let {
                                                                name = "foobar";
                                                                value = ""}
    |}]

let return_input = {|
  return 5;
  return 10;
  return 993322;
|}

let%expect_test "test_return" =
  create_parser return_input |> print_program;
  [%expect
    {|
    Return {value = ""}
    Return {value = ""}
    Return {value = ""}
    |}]
