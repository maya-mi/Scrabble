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

(*......................................................................
  Configuration 
 *)
  

let windowSize = 750;;
let grn = G.rgb 84 139 84;;



(*......................................................................
  A solver that animates the solution process using OCaml's X11
  graphics support
 *)



let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec


let display_message msg = 
  G.moveto (windowSize / 2) (windowSize / 2) ;
  G.set_color G.black;
  G.draw_string msg;
  delay 3.;
  let (x, y) = G.text_size msg in 
  G.set_color grn;
  G.fill_rect (windowSize / 2) (windowSize / 2) x y;;

let x11_initialize () =
  (* open a graphics window to draw into and size it appropriately *)
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
  if p = 1 then (G.draw_string " 0 AI assigned"; delay 3.; (1, 0)) else
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

 
  (* turn off auto synchronizing; we'll handle double buffer
     synchronization ourselves *)

 



(*
let listen () = 
	x11_initialize ();
	let b = new board in 
  	b#draw ();
	try
		loop_at_exit [Graphics.Button_down; Poll] (fun s -> sprintf "Mouse position: %d,%d" s.mouse_x s.mouse_y);
	with
	| End -> x11_finalize ();; *)


(*
let () =
  open_graph "";
  loop ();
  close_graph ();*)
(*
let listen () = 
	x11_initialize ();
	let b = new board in 
	b#init ();
  	b#draw ();
  	let rec loop () =
 		let s = wait_next_event [Button_down] in
  		clear_graph ();
  		b#react s;
 		b#draw (); 
  		loop () in 
	loop ();
	x11_finalize ();;*)

let listen () = 
	let (p, a) = x11_initialize () in 
	let b = new board p a in 
	b#init ();
	b#draw (); 
	loop_at_exit [Button_down; Key_pressed] (fun s -> b#react s; clear_graph (); b#draw ());;

listen ();;





