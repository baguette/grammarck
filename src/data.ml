
type element =
  | Terminal of string
  | Nonterminal of string

type production = Prod of element * (element list)


let element_repr elem =
  match elem with
  | Terminal(s) -> "Terminal(" ^ s ^ ")"
  | Nonterminal(s) -> "Nonterminal(" ^ s ^ ")"

let production_repr prods =
  let Prod(elem, elems) = prods in
  let rec loop elems =
    match elems with
    | []    -> ""
    | x::[] -> (element_repr x)
    | x::xs -> (element_repr x) ^ " " ^ (loop xs)
  in
    (element_repr elem) ^ " : " ^ (loop elems)

let rec production_list_repr prods =
  match prods with
  | []    -> ""
  | x::xs -> (production_repr x) ^ "\n" ^ (production_list_repr xs)

