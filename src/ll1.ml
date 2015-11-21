open Data
open Funcs

module P = Set.Make(Production)

let tbl_default t k d =
  match T.find_all t k with
  | [] -> T.add t k d; d
  | x::_ -> x

let find_default t k d =
  match T.find_all t k with
  | [] -> d
  | x::_ -> x


let compute_parse_table productions =
  let t = T.create @@ List.length productions in
  let first, follow, nullable = compute_first_follow productions in

  let rec first_strand elms =
    match elms with
    | [] -> S.empty
    | x::xs ->
      if T.mem nullable x then
        S.union (find_default first x S.empty) (first_strand xs)
      else
        find_default first x S.empty
  in

  List.iter (fun prod ->
    let Prod(name, elms) = prod in
    let row = tbl_default t name @@ T.create @@ List.length productions in
    
    S.iter (fun term ->
      let cell = tbl_default row term P.empty in
      T.replace row term @@ P.union cell @@ P.add prod P.empty
    ) @@ first_strand elms;

    if List.for_all (T.mem nullable) elms then
      S.iter (fun term ->
        let cell = tbl_default row term P.empty in
        T.replace row term @@ P.union cell @@ P.add prod P.empty
      ) @@ find_default follow name S.empty;

  ) productions;

  t


let report_conflicts productions =
  let conflicts = ref 0 in

  T.iter (fun k1 t ->
    T.iter (fun k2 v ->
      if P.cardinal v > 1 then (
        conflicts := !conflicts + 1;
        Printf.printf "Conflict for %s, %s: \n"
            (element_to_string k1) (element_to_string k2);
        P.iter (fun x ->
          Printf.printf "%s\n" @@ production_to_string x
        ) v;
        Printf.printf "\n";
      );
    ) t;
  ) @@ compute_parse_table productions;
  
  if !conflicts > 0 then
    Printf.printf "Found %d conflicts\n" !conflicts
  else
    Printf.printf "Grammar looks good to me!\n"


let print_parse_table productions =
  T.iter (fun k1 t ->
    T.iter (fun k2 v ->
      Printf.printf "%s, %s: { " (element_to_string k1) (element_to_string k2);
      P.iter (fun x ->
        Printf.printf "%s, " @@ production_to_string x
      ) v;
      Printf.printf "}\n";
    ) t;
  ) @@ compute_parse_table productions

