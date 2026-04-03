let () =
  Printf.printf
    "Welcome to the Zoo Programming Language!\nFeel free to type in commands\n";
  Zoo.Repl.start stdin stdout
