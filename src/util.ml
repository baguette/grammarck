(* Utility functions *)

(* Discard the first n elements of xs *)
let rec drop n xs =
  match xs with
  | [] -> []
  | x::xs as rest -> if n <= 0 then
                       rest
                     else
                       drop (n - 1) xs

(* Get the first n elements of xs *)
let rec take n xs =
  match xs with
  | [] -> []
  | x::xs -> if n <= 0 then
               []
             else
               x :: take (n - 1) xs

(* Like List.for_all, but return false when the list is empty *)
let each pred xs =
  match xs with
  | [] -> false
  | xs -> List.for_all pred xs


