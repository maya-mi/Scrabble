(*
            Solving and rendering force-directed graphics
                          CS51 Problem Set 7
                         -.-. ... ..... .----
 *)
open Graphics ;;
open Printf ;;
open Board;;
open Tile;;

(*......................................................................
  Configuration 
 *)
  
(* Minimum time between displaying successive frames *)
let cFRAMEDELAY = 1. /. 1000. ;;
(* Macimum number of frames (time steps) before giving up on reaching
   quiescence *)
let cMAXFRAMES = 2000 ;;

let cSTEPPING = ref false ;;
let windowSize = 750;;




(*......................................................................
  A solver that animates the solution process using OCaml's X11
  graphics support
 *)

module G = Graphics ;;

let rec delay (sec: float) : unit =
  try ignore(Thread.delay sec)
  with Unix.Unix_error _ -> delay sec

let framedelay () = delay cFRAMEDELAY ;;

let x11_initialize () =
  (* open a graphics window to draw into and size it appropriately *)
  G.open_graph "";
  G.resize_window windowSize windowSize;;
  (* turn off auto synchronizing; we'll handle double buffer
     synchronization ourselves *)



let x11_finalize () =
  (* Close the window on keystroke *)
  ignore (G.read_key ()) ;;


exception End;;  



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
	x11_initialize ();
	let b = new board in 
	b#init ();
	b#draw (); 
	loop_at_exit [Button_down; Key_pressed] (fun s -> clear_graph (); b#react s; b#draw ());;

listen ();;





