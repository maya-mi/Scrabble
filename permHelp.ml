let rec interleave e seen l =
  match l with
  | [] -> [seen @ [e]]
  | x::xs -> (seen @ e :: x :: xs) :: interleave e (seen @ [x]) xs
;;



let combine x ps =
  List.concat (List.map (interleave x []) ps)
;;


let rec perm1 p =
  match p with
  | [] -> [[]]
  | h::t -> combine h (perm1 t)
;;



print_int (List.length (perm1 [0; 1; 2; 3; 4; 5; 6; 7; 8])) ;;