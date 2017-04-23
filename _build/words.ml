

(*We construct letters as records containing their char and score values*)
type letter = {id: char; score: int};;

let onePoint = ["a"; "e"; "i"; "o"; "n"; "r"; "t"; "l"; "s"; "u"] ;;
let twoPoint = ["d"; "g"] ;;
let threePoint = ["b"; "c"; "m"; "p"] ;;
let fourPoint = ["f"; "h"; "v"; "w"; "y"] ;;
let fivePoint = ["k"] ;;
let eightPoint = ["j"; "x"] ;;
let tenPoint = ["q"; "z"] ;;

let detVal (input : string) : int =
  if List.mem input onePoint then 1
  else if List.mem input twoPoint then 2
  else if List.mem input threePoint then 3
  else if List.mem input fourPoint then 4
  else if List.mem input fivePoint then 5
  else if List.mem input eightPoint then 8
  else 10
;; 

let rec range (min : int) (max : int) : int list =
  if min > max then []
  else min :: range (min + 1) max ;;

let abc = List.map char_of_int (range 65 90);;
let freq = [9; 2; 2; 4; 12; 2; 3; 2; 9; 1; 1; 4; 2; 6; 8; 2; 1; 6; 4; 6; 4; 2; 2; 1; 8; 1];;

let rec letterAdd acc x c = 
  if x > 0 then 
    letterAdd ({id = c; score = (detVal (String.lowercase (Char.escaped c)))} :: acc) (x - 1) c
  else acc;;

let fullSet  = List.fold_left2 letterAdd [] freq abc;;

(*http://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml*)
let shuffle lst =
  Random.self_init (); 
  let withRandom = List.map (fun x -> (Random.bits (), x)) lst in
  List.map snd (List.sort compare withRandom);;


type word = letter list;;

open Hashtbl ;;

let buildTable (unit) : (string, int) Hashtbl.t  =
  let result = create 100000 in
  let reader = open_in "ospd.txt" in
  let _ = (try
    while true do
      let temp = input_line reader in
      add result temp (hash temp)
    done
  with
    End_of_file -> ()) in
  result
;;


let dict = buildTable () ;;


(* List.fold_right, except folding over a string *)
let rec sFold (f : string -> 'a -> 'a) (acc : 'a) (s : string) : 'a =
  match s with
  | "" -> acc
  | _ -> let l = String.length s in
  	sFold f (f (String.sub s (l - 1) 1) acc) (String.sub s 0 (l - 1))
;;

let makeWord (input : string) : word =
  sFold (fun x y -> {id = String.get x 0; score = detVal x} :: y) [] input
;;
  

let getChar ({id = a; score = _} : letter) : string = String.make 1 a ;;

let getVal ({id = _; score = a} : letter) : int = a ;;

let toString (input : word) : string =
  List.fold_left (fun x y -> x ^ (getChar y)) "" input 
;;

let getScore (input : word) : int =
  List.fold_left (fun x y -> x + (getVal y)) 0 input
;;

let isWord (test : word) : bool =
  mem dict (String.lowercase (toString test))
;;

(*let s = makeWord "zoo" in
print_int (getScore s) ;;*)

