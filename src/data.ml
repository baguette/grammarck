
type element =
  | Terminal of string
  | Nonterminal of string

type production = Prod of element * (element list)



let rec find_productions lhs productions =
  match productions with
  | []    -> []
  | prod::prods ->
    let Prod(name, elems) = prod in
    if lhs == name then
      prod :: (find_productions lhs prods)
    else
      find_productions lhs prods



let element_to_string elem =
  match elem with
  | Terminal(s) -> s
  | Nonterminal(s) -> s

let production_to_string prods =
  let Prod(elem, elems) = prods in
  let rec loop elems =
    match elems with
    | []    -> ""
    | x::[] -> (element_to_string x)
    | x::xs -> (element_to_string x) ^ " " ^ (loop xs)
  in
    (element_to_string elem) ^ " : " ^ (loop elems)

let rec production_list_to_string prods =
  match prods with
  | []    -> ""
  | x::xs -> (production_to_string x) ^ "\n" ^ (production_list_to_string xs)



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



module Element = struct
  type t = element
  let compare = compare
end

module Production = struct
  type t = production
  let compare = compare
end

