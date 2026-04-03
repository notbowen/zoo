let start in_c out_c =
  let () = Printf.fprintf out_c ">>> %!" in
  let input = input_line in_c in
  let lexer = Lexer.init input in
  let rec parse lex =
    let res = Lexer.next_token lex in
    match res with
    | None -> ()
    | Some (lex, tok) ->
        let () = Printf.fprintf out_c "%s\n" (Token.show tok) in
        parse lex
  in
  parse lexer
