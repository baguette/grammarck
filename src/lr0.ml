open Data
open Funcs

module I = Set.Make(LR0_Item)
module St = Set.Make(I)

type edge = Edge of I.t * element * I.t

module Edge_compare = struct
  type t = edge
  let compare = compare
end

module E = Set.Make(Edge_compare)



let state_to_string state =
  "{ " ^
  (String.concat ", " @@ List.map lr0_item_to_string @@ I.elements state) ^
  " }"



type action = Shift of I.t
            | Goto of I.t
            | Reduce of production
            | Accept
            | Error

let action_repr = function
  | Shift(n)  -> Printf.sprintf "Shift(%s)"  @@ state_to_string n
  | Goto(n)   -> Printf.sprintf "Goto(%s)"   @@ state_to_string n
  | Reduce(k) -> Printf.sprintf "Reduce(%s)" @@ production_to_string k
  | Accept    -> "Accept"
  | Error     -> "Error"

module Action = struct
  type t = action
  let compare = compare
end

module A = Set.Make(Action)



let item_at itm =
  let Item0(n, Prod(lhs, rhs)) = itm in
  let len = List.length rhs in
  if len = 0 then
    None
  else if n < len then
    Some(List.nth rhs n)
  else
    None

let item_is_at_nonterminal itm =
  match item_at itm with
  | None -> false
  | Some(Nonterminal(_)) -> true
  | _ -> false

let item_advance itm =
  let Item0(n, prod) = itm in
  Item0(n + 1, prod)

let item_is_at_end itm =
  let Item0(n, Prod(lhs, rhs)) = itm in
  n >= List.length rhs



let closure productions itms =
  let citms = ref itms in
  let changed = ref true in

  while !changed do
    changed := false;
    I.iter (fun itm ->
      if item_is_at_nonterminal itm then
        let curr_item = item_at itm in
        match curr_item with
        | None -> ()
        | Some(curr_item) ->
          List.iter (fun prod ->
            let nitm = Item0(0, prod) in
            if not (I.mem nitm !citms) then (
              citms := I.add nitm !citms;
              changed := true;
            );
          ) @@ find_productions curr_item productions;
    ) !citms;
  done;

  !citms

let goto productions itms x =
  let gitms = ref I.empty in

  I.iter (fun itm ->
    let curr_item = item_at itm in
    match curr_item with
    | None -> ()
    | Some(curr_item) ->
      if x = curr_item then
        gitms := I.add (item_advance itm) !gitms;
  ) itms;
  
  closure productions !gitms



(* Compute the DFA used to recognize handles on the stack *)
let compute_dfa start productions =
  (* Augment the grammar with a new start production S' : S$ *)
  let new_start = Prod(Nonterminal("S'"), [start; Terminal("$")]) in
  let productions = new_start :: productions in

  (* t is the set of states seen so far, starting with the new start *)
  let init = I.singleton @@ Item0(0, new_start) in
  let t = ref @@ St.singleton @@ closure productions init in

  (* e is the set of (shift or goto) edges found so far *)
  let e = ref E.empty in

  let changed = ref true in
  while !changed do
    changed := false;

    St.iter (fun state ->
      I.iter (fun itm ->
        let elem = item_at itm in

        match elem with
        | None -> ()
        | Some(Terminal("$")) -> ()
        | Some(elem) ->
          let nitm = goto productions state elem in
          let ne = Edge(state, elem, nitm) in

          if not (St.mem nitm !t) then (
            t := St.union !t @@ St.singleton nitm;
            changed := true
          );
          if not (E.mem ne !e) then (
            e := E.union !e @@ E.singleton ne;
            changed := true
          );
      ) state
    ) !t
  done;

  (!t, !e)



let replace tbl k v =
  match T.find_all tbl k with
  | []     -> T.replace tbl k @@ A.singleton v
  | x::[]  -> T.replace tbl k @@ A.union x @@ A.singleton v
  | _      -> failwith "table got fubar"


(* Compute shift and goto actions given edges e and add them to tbl *)
let edge_actions e tbl =
  E.iter (fun edge ->
    let Edge(i, x, j) = edge in
    match x with
    | Terminal(_)    -> replace tbl (i, x) @@ Shift(j)
    | Nonterminal(_) -> replace tbl (i, x) @@ Goto(j)
  ) e

(* Compute accept actions given states t and add them to tbl *)
let accept_actions t tbl =
  St.iter (fun state ->
    I.iter (fun itm ->
      let curr_item = item_at itm in
      match curr_item with
      | None -> ()
      | Some(curr_item) ->
        if curr_item = Terminal("$") then
          replace tbl (state, Terminal("$")) Accept;
    ) state
  ) t

(* Compute reduce actions given states t, productions and add them to tbl *)
let reduce_actions t productions tbl =
  let tokens = get_terminals productions in
  St.iter (fun state ->
    I.iter (fun itm ->
      if item_is_at_end itm then
        let Item0(_, prod) = itm in
        S.iter (fun tok ->
          replace tbl (state, tok) @@ Reduce(prod)
        ) tokens
    ) state
  ) t



let compute_parse_table start productions =
  let tbl = T.create @@ 2 * List.length productions in
  let t, e = compute_dfa start productions in
  edge_actions e tbl;
  reduce_actions t productions tbl;
  accept_actions t tbl;
  tbl



let report_conflicts start productions =
  let tbl = compute_parse_table start productions in
  let count = ref 0 in
  T.iter (fun k v ->
    if A.cardinal v > 1 then (
      let (i, x) = k in
      Printf.printf "Conflict for ";
      Printf.printf "%s" @@ state_to_string i;
      Printf.printf ", %s:\n" @@ element_to_string x;
      A.iter (fun action ->
        Printf.printf "%s\n" @@ action_repr action
      ) v;
      Printf.printf "\n";
      incr count
    );
  ) tbl;
  if !count > 0 then
    Printf.printf "Found %d conflicts\n" !count
  else
    Printf.printf "Grammar looks good to me!\n"



let print_edges start productions =
  let t, e = compute_dfa start productions in
  E.iter (fun edge ->
    let Edge(i, x, j) = edge in
    Printf.printf "%s --%s--> %s\n" (state_to_string i)
                                    (element_to_string x)
                                    (state_to_string j)
  ) e

let print_parse_table start productions =
  let tbl = compute_parse_table start productions in

  T.iter (fun k v ->
    let (i, x) = k in
    Printf.printf "[%s, %s]:\n" (state_to_string i) (element_to_string x);
    A.iter (fun action ->
      Printf.printf "%s\n" @@ action_repr action
    ) v;
    Printf.printf "\n"
  ) tbl

