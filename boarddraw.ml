(*
            Solving and rendering force-directed graphics
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)
open Graphics ;;
open Printf ;;
open Board ;;
open Tile ;;
module G = Graphics ;;

(*......................................................................*)
  

let windowSize = 750;;
let grn = G.rgb 84 139 84;;



let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec



(* Open a graphics window to draw into and size it appropriately. Listens
for user preferences for number of total playters, including AIs, and
number of AIs*)
let initialize () =
  G.open_graph "";
  G.resize_window windowSize windowSize;
  G.set_color grn;
  G.fill_rect 0 0 windowSize windowSize;
  G.set_color G.black;
  G.set_text_size 300;
  G.moveto (windowSize / 3) (3 * windowSize / 4) ;
  G.draw_string "Welcome to Scrabble. Enter number of players: [1-4]";
  let rec player_loop () = 
    try 
      (let k = int_of_string (Char.escaped (G.read_key ())) in 
      if k > 0 && k < 5 then k else player_loop ();)
    with Failure "int_of_string" -> player_loop (); in 
  let p = player_loop () in 
  G.moveto (windowSize / 3) (2 * windowSize / 3);
  G.draw_string ((string_of_int p) ^ " players selected.");
  if p = 1 then (G.draw_string " 0 AI assigned"; delay 1.; (1, 0)) else
  (G.moveto (windowSize / 3) (windowSize / 2);
  G.draw_string ("Enter number of AI: [0-" ^ (string_of_int (p - 1)) ^ "]");
  let rec ai_loop () = 
    try 
      (let k = int_of_string (Char.escaped (G.read_key ())) in 
      if k >= 0 && k < p then k else ai_loop ();)
    with Failure "int_of_string" -> ai_loop (); in 
  let a = ai_loop () in 
  G.moveto (windowSize / 3) (5 * windowSize / 12);
  G.draw_string ((string_of_int a) ^ " AI selected.");
  delay 1.;
  (p, a));;

 
(*Initializes board with appt players. Listens for user interaction*)
let listen () = 
	let (p, a) = initialize () in 
	let b = new board p a in 
	b#init ();
	b#draw (); 
	loop_at_exit [Button_down; Key_pressed] 
    (fun s -> b#react s; clear_graph (); b#draw ());;

listen ();;





