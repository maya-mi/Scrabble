open Words ;;
open Tile ;;


let getWords (board : tile array array) (tiles : tile array) (timer : float) : (tile * int * int) list = 
  (*let bestWord = [] in
  let _ = while timer > 0. do
    for i = 0 to Array.length board - 1 do
      for x = 0 to Array.length board.(i) - 1 do
        if isLegal 
      done
    done 
  done in
  []*)
  failwith "not yet implemented"
;;



(*let isHori (tiles : tile array) : bool option = 
  let hor = ref true in 
  let vert = ref true in
  let lastX = tiles.(0)#getX in
  let lastY = tiles.(0)#getY in
  let _ = for i = 1 to Array.length tiles - 1 do 
    if tiles.(i)#getX <> lastX + 1 || tiles.(i)#getY <> lastY then hor := false ;
    if tiles.(i)#getY <> lastY + 1 || tiles.(i)#getX <> lastX then vert := false
  done in
  if !hor then Some true else if !vert then Some false else None
;;


let compileWord (board : tile array array) (hor : bool) (start : tile) : word =
  let result = ref [] in
  if hor then
    (let cur = ref start#getY in
    while !cur < Array.length board && not board.(start#getY).(!cur)#isBlank do
      result := !result @ [board.(start#getx).(!cur)#getLetter] ;
      cur := !cur + 1
    done ;
    cur := start#gety - 1 ;
     while cur >= 0 && not board.(start#getX).(!cur)#isBlank do
      result := board.(start#getX).(!cur)#getLetter :: !result ;
      cur := !cur - 1 ; 
    done ;
    !result)
  else
    (let cur = ref start#getx in
    while !cur < Array.length board && not board.(!cur).(start#gety)#isBlank do
      result := !result @ [board.(cur).(start#getY)#getLetter] ;
      cur := !cur + 1
    done ;
    cur := tile#getX - 1 ;
     while !cur >= 0 && not board.(!cur).(start#getY)#isBlank do
      result := board.(!cur).(start#gety)#getLetter :: !result ;
      cur := !cur - 1 ; 
    done ;
    !result)
;;



let computeVal (board : tile array array) (tiles : tile array) : int =
  let result = 0 in
  match isHori tiles with
  | Some a -> 
    let _ = for i = 0 to Array.length tiles - 1 do
      let word1 = compileWord board (not a) tiles.(i) in
      if not isWord word1 then -1 else
      ignore result = result + (getScore word1)
    done in result
  | None -> -1
;;*)





let rec interleave_help (x: int) (l: int list) (r: int list) (combos: int list list): (int list) list = 
  match r with
  |[] -> (l @ [x]) :: combos
  |hd :: tl -> interleave_help x (l @ [hd]) tl (((l @ [x]) @ r) :: combos);;

(*Passes the interwoven x and list of interest to interleave_help, along
with the initially empty list representing the values to the left of x*)
let interleave (x: int) (lst: int list): (int list) list = 
  interleave_help x [] lst [];;

(*Recursively interleaves the current first element of the int list with
the permutations of all previous elements, using List.map and List.concat*)
let rec perm_help (ints: int list) (combos: int list list) = 
  match ints with
  |[] -> combos
  |hd:: tl -> perm_help tl (List.concat (List.map (fun lst -> interleave hd lst) combos))
;;

(*Passes the int list and a list of combos of [], or [[]], to perm_help*)
let permutations (ints: int list) : (int list) list =
	perm_help ints [[]]
;;

let standPerm (n : int) : (int list) list =
  let rec listGen (a : int) : int list = 
    if a >= n then [] else a :: (listGen (a + 1))
  in
  permutations (listGen 0)
;;

let printListList (a : (int list) list) : unit = 
  List.iter (fun x -> List.iter (fun a -> print_int a; Printf.printf " ") x; print_endline "") a
;;

let perms = Array.make 10 [[]] in 
  for i = 0 to 9 do
    Array.set perms i (standPerm i)
  done 
;;

let permTiles (tiles : tile array) : unit =
	let perms = standPerm (Array.length tiles) in ()
;;

let bestWordH (board : tile array array) (tiles : tile array) ((x,y) : int * int) : (tile * int * int) list =
  failwith "not yet implemented"

;;

exception OffBoard ;;

let scoreTiles (move : (tile * int * int) list) : int =
  try 
    let toWord (letters : (tile * int *int) list) : word =
      List.fold_right (fun n acc -> 
        match n with
        | t, x, y -> if min x y < 0 || max x y >= 15 then raise OffBoard else
          t#getLetter :: acc) letters []
    in
    let attempt = toWord move in
    if not (isWord attempt) then -1 else getScore attempt 
  with OffBoard -> -1
;;