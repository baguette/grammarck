open Data
open Util

module T = Hashtbl
module S = Set.Make(Element)
module P = Set.Make(Production)


(* Union the production sets found at tbl1[k1] and tbl2[k2] 
 * such that tbl1[k1] = tbl1[k1] U tbl2[k2]
 *
 * Returns true if tbl1[k1] was changed from its previous value,
 * false otherwise.
 *)
let union tbl1 k1 tbl2 k2 : bool =
  (* If tbln does not have kn, initialize it to the empty set *)
  let _ = match T.find_all tbl1 k1 with
          | [] -> T.add tbl1 k1 S.empty
          | _ -> () in

  let _ = match T.find_all tbl2 k2 with
          | [] -> T.add tbl2 k2 S.empty
          | _ -> () in

  let prev = T.find tbl1 k1 in
  let next = T.find tbl2 k2 in

  T.replace tbl1 k1 @@ S.union prev next;

  let u = T.find tbl1 k1 in

  if S.compare u prev <> 0 then
    true
  else
    false


let rec find_productions lhs productions =
  List.filter (fun prod ->
    let Prod(name, elems) = prod in
    lhs = name
  ) productions


(* True if elem is a terminal *)
let is_terminal elem =
  match elem with
  | Terminal(_) -> true
  | _ -> false

(* Get a set of all terminals that appear in a production list *)
let rec get_terminals productions =
  match productions with
  | []    -> S.empty
  | Prod(name, elems)::prods ->
    List.fold_right S.add (List.filter is_terminal elems)
                          (get_terminals prods)



(* Algorithm 3.13 from p49 of Appel (1998)
 *
 * Compute FIRST, FOLLOW, and nullable sets for a given grammar
 * represented by production list productions
 *
 * FIRST and FOLLOW are Hashtbls of the form
 *    tbl[element] = Set of element (S.t)
 *
 * nullable is also a Hashtbl, but containing only elements which
 * are nullable:
 *     nullable[element] = true
 * If an element is not nullable, it will not be mapped in the table.
 *)
let compute_first_follow productions =
  let len = List.length productions in

  let first = T.create len
  and follow = T.create len
  and nullable = T.create len in

  let _ = S.iter (fun a ->
    T.replace first a @@ S.add a S.empty;
  ) (get_terminals productions) in
  
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun prod ->
      let Prod(name, elems) = prod in
      match elems with
      | [] -> if not (T.mem nullable name) then (
                T.add nullable name true;
                changed := true
              );

      | elms -> if each (T.mem nullable) elms then
                  if not (T.mem nullable name) then (
                    T.add nullable name true;
                    changed := true
                  );

                for i = 0 to (List.length elms) - 1 do
                  if i = 0
                  || each (T.mem nullable) (take (i-1) elms)
                  then
                    changed := union first name
                                     first (List.nth elms i);

                  for j = i + 1 to (List.length elms) - 1 do
                    if i = (List.length elms) - 1
                    || each (T.mem nullable) (drop i elms)
                    then
                      changed := union follow (List.nth elms i)
                                       follow name;

                    if i + 1 = j
                    || each (T.mem nullable) (take (j-1) (drop (i+1) elms))
                    then
                      changed := union follow (List.nth elms i)
                                       first  (List.nth elms j);
                  done
                done
    ) productions
  done;

  (first, follow, nullable)



(* Print a Set of elements (S.t) *)
let print_set s =
  print_string "{ ";
  S.iter (fun x ->
    print_string @@ element_to_string x;
    print_string " "
  ) s;
  print_string "}"

(* Print a FIRST or FOLLOW table *)
let print_tbl t =
  T.iter (fun k v ->
    match k with
    | Terminal(_) -> ()
    | Nonterminal(k) ->
      print_string k;
      print_string " ";
      print_set v;
      print_string "\n"
  ) t

