open Util


type element =
  | Terminal of string
  | Nonterminal of string

type production = Prod of element * (element list)

type lr0_item = Item0 of int * production
type lr1_item = Item1 of int * production * element




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

let lr0_item_to_string itm =
  let Item0(n, prod) = itm in
  let Prod(lhs, elms) = prod in
  let head = take n elms
  and tail = drop n elms in

  (element_to_string lhs) ^ " : " ^
  String.concat " " (List.map element_to_string head) ^
  " . " ^
  String.concat " " (List.map element_to_string tail)

let lr1_item_to_string itm =
  let Item1(n, prod, k) = itm in
  let prefix = Item0(n, prod) in
  (lr0_item_to_string prefix) ^ ", " ^ (element_to_string k)



let element_repr elem =
  match elem with
  | Terminal(s) -> "Terminal(" ^ s ^ ")"
  | Nonterminal(s) -> "Nonterminal(" ^ s ^ ")"

let production_repr prods =
  let Prod(elem, elems) = prods in
  (element_repr elem) ^ " : " ^
  String.concat " " (List.map element_repr elems)

let rec production_list_repr prods =
  match prods with
  | []    -> ""
  | x::xs -> (production_repr x) ^ "\n" ^ (production_list_repr xs)

let lr0_item_repr itm =
  let Item0(n, prod) = itm in
  Printf.sprintf "[%d] %s" n @@ production_repr prod

let lr1_item_repr itm =
  let Item1(n, prod, k) = itm in
  let prefix = Item0(n, prod) in
  (lr0_item_repr prefix) ^ ", " ^ (element_to_string k)



module Element = struct
  type t = element
  let compare = compare
end

module Production = struct
  type t = production
  let compare = compare
end

module LR0_Item = struct
  type t = lr0_item
  let compare = compare
end

module LR1_Item = struct
  type t = lr1_item
  let compare = compare
end
