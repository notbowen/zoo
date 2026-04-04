type node =
  | Program of program
  | Statement of statement
  | Expression of expression
[@@deriving show { with_path = false }]

and expression = string

and statement =
  | Let of { name : identifier; value : expression }
  | Return of { value : expression }

and identifier = string
and program = statement list
