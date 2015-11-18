
type tokens =
  | Ident of string
  | Colon
  | Pipe
  | Semi
  | Sep
  | Ptoken

let token_repr tok =
  match tok with
  | Ident(s) -> "Ident(" ^ s ^ ")"
  | Colon    -> "Colon"
  | Pipe     -> "Pipe"
  | Semi     -> "Semi"
  | Sep      -> "Sep"
  | Ptoken   -> "Ptoken"

