open Tile;;
open Words;;
open PermHelp ;;
open Graphics;;

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 18;;
let blank = new tile {id = char_of_int 32; score = 0};;
let w2 = (new tile {id = char_of_int 32; score = 0});;
w2#setWordMult 2;;
let l2 = (new tile {id = char_of_int 32; score = 0});;
l2#setLetterMult 2;;
let w3 = (new tile {id = char_of_int 32; score = 0});;
w3#setWordMult 3;;
let l3 = (new tile {id = char_of_int 32; score = 0});;
l3#setLetterMult 3;;
let grn = Graphics.rgb 84 139 84;;

let listFind f lst = 
	match lst with
	|hd :: tl -> List.fold_left f hd tl
	|[] -> failwith "Empty";;

let compare_all tup_lst = 
	let compare lst =
		match lst with
		|[] -> true
		|hd :: tl -> List.fold_left (fun acc x -> acc && x = hd) true tl in 
	let xs, ys = List.split tup_lst in
	(compare xs, compare ys);;

let inRange x l m = 
	x >= l && x <= m;;

class board (players: int) (ais:int) = 
	object (this)

	val mutable layout = Array.make_matrix 15 15 blank
	val mutable drawPile = shuffle fullSet
	val mutable hands = Array.make_matrix players 7 blank
	val mutable reals = Array.make players false
	val mutable savedQ = min_int
	val mutable toggleClicked = false
	val mutable play = []
	val perms = perm1 [0; 1; 2; 3; 4; 5; 7; 8]
	val mutable standL = [0; 1; 2; 3; 4; 5; 6; 7; 8]
	val mutable validPos = false
	val mutable scores = Array.make players 0
	val mutable turn = 0
	val mutable passes = 0
	val mutable turnScore = 0	
	val mutable dumping = false
	val mutable help = false
	val mutable dumps = []
	val w2s = [(1, 1); (2, 2); (3, 3); (4, 4); (10, 10); (11, 11); (12, 12); (13, 13);
			   (1, 13); (2, 12); (3, 11); (4, 10); (10, 4); (11, 3); (12, 2); (13, 1)]
    val l2s = [(0, 3); (0, 11); (3, 0); (11, 0); (14, 3); (14, 11); (3, 14); (11, 14);
    		   (6, 6); (8, 8); (8, 6); (6, 8);
    		   (2, 6); (2, 8); (3, 7); (6, 2); (8, 2); (7, 3);
    		   (12, 6); (12, 8); (11, 7); (6, 12); (8, 12); (7, 11)]
    val w3s = [(0, 0); (7, 0); (14, 0); (0, 7); (0, 14); (7, 14); (14, 7); (14, 14)]
    val l3s = [(5, 5); (1, 5); (1, 9); (5, 9); (5, 13); (9, 13); (9, 9); (13, 9); (13, 5); (9, 5); (9, 1); (5, 1)]
    		   

	method pullTile () = 
		match drawPile with
		|[] -> blank
		|hd :: tl -> drawPile <- tl; new tile hd

	method drawSetting () = 
		Graphics.set_color grn;
		Graphics.fill_rect 0 0 cFRAMESIZE cFRAMESIZE;
		Graphics.set_color (Graphics.black);
		Array.iteri (fun index x -> 
		Graphics.moveto (length * index * 7 / 2 + length) (cFRAMESIZE - length);
		Graphics.draw_string ("PLAYER " ^ (string_of_int (index + 1)) ^ "'S SCORE: " ^ (string_of_int x))) scores ;
		Graphics.moveto (cFRAMESIZE - 3 * length) (cFRAMESIZE - length);
		let msg = "PLAYER " ^ (string_of_int (1 + turn)) ^ "'S TURN" in 
		let (x, y) = text_size msg in 
		draw_string msg; 
		set_color red;
		draw_rect (cFRAMESIZE - 3 * length - 5) (cFRAMESIZE - length - 5) (x + 10) (y + 10)



	method drawHelp () = 
		this#drawSetting ();
		l2#draw 1 3;
		l3#draw 1 5;
		w2#draw 1 8;
		w3#draw 1 10;
		Graphics.set_color Graphics.black;
		Graphics.moveto (cFRAMESIZE / 5) (length * 25 / 2);
		Graphics.draw_string "Triple word score tile";
		Graphics.moveto (cFRAMESIZE / 5) (length * 21 / 2);
		Graphics.draw_string "Double word score tile";
		Graphics.moveto (cFRAMESIZE / 5) (length * 15 / 2);
		Graphics.draw_string "Triple letter score tile";
		Graphics.moveto (cFRAMESIZE / 5) (length * 11 / 2);
		Graphics.draw_string "Double letter score tile";
		let msgs = ["'p' passes"; 
					"Enter 'd' to select tiles to dump, and 'd' to finalize dump";
					"'r' resets the board, including unfinished dumps"; "Hit the space bar to score a word"; "'x' exits the game"; "PRESS 'h' TO EXIT HELP"] in 
		List.iteri (fun i msg -> Graphics.moveto (cFRAMESIZE / 2) (cFRAMESIZE - length * (i + 7)); Graphics.draw_string msg) msgs


	method drawBoard () = 
		this#drawSetting();
		for i = 0 to 14 do
		  for j = 0 to 14 do 
		    layout.(i).(j)#draw i j;
		  done
		done;


	method dump numL =
		let adds = List.fold_left (fun x y -> let a = hands.(turn).(y) in hands.(turn).(y) <- this#pullTile (); a :: x) [] numL in
		drawPile <- List.fold_left (fun x y -> y#getLetter :: x) drawPile adds ;
		drawPile <- shuffle drawPile;
		this#reset ();


	method drawHand () = 
		for i = 0 to 6 do
			hands.(turn).(i)#draw 16 (i + 4);
		done


	method draw () = 
		if help then this#drawHelp ()
		else
			(this#drawBoard ();
			this#drawHand ())

	method init () = 
		List.iter (fun (x, y) -> layout.(x).(y) <- w2) w2s;
		List.iter (fun (x, y) -> layout.(x).(y) <- l2) l2s;
		List.iter (fun (x, y) -> layout.(x).(y) <- w3) w3s;
		List.iter (fun (x, y) -> layout.(x).(y) <- l3) l3s;
		for i = 0 to players - 1 do
			for j = 0 to 6 do 
				hands.(i).(j) <- this#pullTile ();
			done;
		done;
		for i = 0 to players - ais - 1 do 
			reals.(i) <- true;
		done;

    method break (point : int) (input : int list) (acc : int list) : (int list) * (int list) =
      match input with
      | h :: t -> if h = point then (acc, t) else this#break point t (h :: acc)
      | [] -> failwith "False case"
(*layout.(x1).(y1) = blank then (layout.(x1).(y1) <- posHand.(h); play <- (x1, y1) :: play;*)
	method playAI posHand =
	  (*this#draw ();
	  moveto (cFRAMESIZE - 2 * length) (cFRAMESIZE - 2 * length);
	  draw_string "AI THINKING";*)
	  let best = ref [] in
	  let bPerm = ref [] in
	  let bScore = ref 0 in
	  let checkPlay curPerm = if List.length play = 0 then this#reset () else if this#is_valid () then (if turnScore > !bScore then best := play; bPerm := (List.fold_left (fun acc cord -> let x,y = cord in layout.(x).(y) :: acc) [] play));
	    this#reset ()
	  in
	  let tryMove (order : int list): unit =
	    let pre, post = this#break 8 order [] in
	    let rec compWord (back : bool) (hor : bool) (x1 : int) (y1 : int) (places : int list) : unit =
	      match places with
	      | h :: t -> if not (x1 >= 0 && x1 <= 14 && y1 >= 0 && y1 <= 14) || h == 7 then () else
	        (if layout.(x1).(y1)#isBlank then (layout.(x1).(y1) <- posHand.(h); play <- (x1, y1) :: play;
              (if (not back) && hor then compWord back hor (x1 + 1) y1 t
	          else if back && hor then compWord back hor (x1 - 1) y1 t
	          else if not back && not hor then compWord back hor x1 (y1 + 1) t
	          else compWord back hor x1 (y1 - 1) t)) 
	        else 
	          (if not back && hor then compWord back hor (x1 + 1) y1 places
	          else if back && hor then compWord back hor (x1 - 1) y1 places
	          else if not back && not hor then compWord back hor x1 (y1 + 1) places
	          else compWord back hor x1 (y1 - 1) places))
	      | [] -> ()
	    in 
	    for x = 0 to 14 do
	      for y = 0 to 14 do
	      if not layout.(x).(y)#isBlank then
            compWord true true x y pre;
            compWord false true x y post ;
            checkPlay order ;
            compWord true false x y pre ;
            compWord false false x y post ;
            checkPlay order;
          done;
        done;
      in (*List.iter tryMove perms;*)
      for _x = 0 to 2000 do
      	standL <- shuffle standL;
        tryMove standL;
      done;
      play <- !best;
      let rec setFinal (coords : (int*int) list) tiles : unit =
      	match coords, tiles with
      	| (x, y) :: t, h1 :: t1 -> let _ = layout.(x).(y) <- h1 in setFinal t t1
	  	| [], [] -> ()
	  	| _ -> failwith "improper behavior"
	  in
	  setFinal play (!bPerm);
	  ignore (this#is_valid ());
	  this#refresh ()




	  	  (*if this#validating x y then
	  	  let rec testMove (order : int list) (posX : int) (posY : int) : unit =
	  	  	if posX >= 15 then (if this#is_valid () then (if turnScore > !bScore then bScore := turnScore; best := play; bPerm := order) ;
	  	    List.iter (fun cur -> let x,y = cur in layout.(x).(y) <- blank) play ; play <- []) else
	  	    match order with
	  	    | [] -> if this#is_valid () then (if turnScore > !bScore then bScore := turnScore; best := play; bPerm := order) ;
	  	    List.iter (fun cur -> let x,y = cur in layout.(x).(y) <- blank) play ; play <- []
	  	    | h :: t -> if h == 7 then (if this#is_valid () then (if turnScore > !bScore then bScore := turnScore; best := play; bPerm := order) ;
	  	    List.iter (fun cur -> let x,y = cur in layout.(x).(y) <- blank) play ; play <- []) 
	  	    else if not layout.(posX).(posY)#isBlank then testMove order (posX + 1) posY else
	  	   	  layout.(posX).(posY) <- posHand.(h); play <- (posX, posY) :: play; 
	  	      testMove t (posX +1) posY
	  	  in
	  	  List.iter (fun cur -> testMove cur x y) perms
	  	done;
	  done;
	  let rec putBack (perm : int list) poses : unit = 
	  	match perm, poses with
	  	| ([], []) -> ()
	  	| h :: t, (x,y) :: t1 -> layout.(x).(y) <- posHand.(h); putBack t t1
	  in putBack !bPerm !best*)






	method advanceTurn () = 
	 let rec help () = 
		turn <- (turn + 1) mod (players);
		if not reals.(turn) then (this#playAI hands.(turn); help ()); in 
	 help ()

	method validating x y = 
		let n = ref [] in 
		if x - 1 > -1 then (n := layout.(x - 1).(y) :: !n);
		if y + 1 < 15 then (n := layout.(x).(y + 1) :: !n);
		if x + 1 < 15 then (n := layout.(x + 1).(y) :: !n);
		if y - 1 > -1 then (n := layout.(x).(y - 1) :: !n);
		let playedTiles = List.map (fun (x, y) -> layout.(x).(y)) play in  
		(x = 7 && y = 7) || List.length (List.filter (fun x -> not (List.mem x playedTiles || x#isBlank)) !n) > 0 

	method mouseClick mouse_x mouse_y= 
		if dumping then
			(let q = mouse_y / length - 6 in 
				if (inRange q 0 6 && inRange mouse_x (cFRAMESIZE - length) cFRAMESIZE) then 
			 		dumps <- q :: dumps;
			    	hands.(turn).(q)#click) 
		else (if toggleClicked then 
			let x = mouse_x / length - 1 in
			let y = mouse_y / length - 2 in
			if (inRange x 0 14 && inRange y 0 14 && layout.(x).(y)#isBlank) then 
				(let wm = layout.(x).(y)#getWordMult
				 in 
				if wm <> 1 then hands.(turn).(savedQ)#setWordMult wm;
				let lm = layout.(x).(y)#getLetterMult in 
				if lm <> 1 then hands.(turn).(savedQ)#setLetterMult lm;
				layout.(x).(y) <- hands.(turn).(savedQ);
				hands.(turn).(savedQ) <- blank;
				play <- (x, y):: play;
				validPos <- validPos || this#validating x y) 
			else (for i = 0 to 6 do
				hands.(turn).(i)#unclick;
			done);
			toggleClicked <- false
		else 
			let q = mouse_y / length - 6 in 
			if (inRange q 0 6 && inRange mouse_x (cFRAMESIZE - length) cFRAMESIZE && not hands.(turn).(q)#isBlank) then 
			 (hands.(turn).(q)#click; savedQ <- q; toggleClicked <- true))

	method addVerts wrd x yMax yMin = 
		let ypos = ref yMax in  
		while !ypos < 14 && not layout.(x).(!ypos + 1)#isBlank do
			wrd := layout.(x).(!ypos + 1) :: !wrd;
			ypos := !ypos + 1;
		done;
		ypos := yMin;
		while !ypos > 0 && not layout.(x).(!ypos - 1)#isBlank do
			wrd := !wrd @ [layout.(x).(!ypos - 1)];
			ypos := !ypos - 1;
		done;
		

	method addHor wrd y xMax xMin = 
		let xpos = ref xMax in  
		while !xpos < 14 && not layout.(!xpos + 1).(y)#isBlank do
			wrd := !wrd @ [layout.(!xpos + 1).(y)];
			xpos := !xpos + 1;
		done;
		xpos := xMin;
		while !xpos > 0 && not layout.(!xpos - 1).(y)#isBlank do
			wrd := layout.(!xpos - 1).(y) :: !wrd;
			xpos := !xpos - 1;
		done;
	
	method vertNormal x y = 
		let wrd = ref [layout.(x).(y)] in 
		this#addVerts wrd x y y;
		if isWord (this#stripLetters !wrd) then (turnScore <- turnScore + (this#score !wrd); true)
		else List.length !wrd = 1 

	method horNormal x y = 
		let wrd = ref [layout.(x).(y)] in 
		this#addHor wrd y x x;
		if isWord (this#stripLetters !wrd) then (turnScore <- turnScore + (this#score !wrd); true)
		else List.length !wrd = 1 
		
	method score (tls: tile list) = 
	 let rec help tls scr mult = 
		match tls with 
		|[] -> scr * mult
		|hd::tl -> help tl (scr + hd#getscore) (mult * hd#getWordMult) in 
	help tls 0 1

	method stripLetters (tls: tile list) = 
	 List.map (fun t -> t#getLetter) tls
	
	method is_valid () = 
	  let xs, ys = List.split play in
	  if List.length play = 1 then 
		  let _ = this#vertNormal (List.hd xs) (List.hd ys) in
		  let _ = this#horNormal (List.hd xs) (List.hd ys) in 
		  turnScore > 0
	  else( 
		let xSame, ySame = compare_all play in
		if xSame then 
			(let wrd = ref [] in 
			let perp = ref true in 
			for i = (listFind min ys) to (listFind max ys) do
				wrd := layout.(List.hd xs).(i) :: !wrd;
				if List.mem ((List.hd xs), i) play then 
					(perp := !perp && this#horNormal (List.hd xs) i;)
			done;
			this#addVerts wrd (List.hd xs) (listFind max ys) (listFind min ys);
			turnScore <- turnScore + (this#score !wrd);
			if List.length play = 7 then turnScore <- turnScore + 50;
			isWord (this#stripLetters !wrd) && !perp && validPos)
		else if ySame then 
			(let wrd = ref [] in 
			let perp = ref true in 
			for i = (listFind min xs) to (listFind max xs) do
				wrd := !wrd @ [layout.(i).(List.hd ys)];
				if List.mem (i, (List.hd ys)) play then 
					(perp := !perp && this#vertNormal i (List.hd ys);)
			done;
			this#addHor wrd (List.hd ys) (listFind max xs) (listFind min xs);
			turnScore <- turnScore + (this#score !wrd);
			if List.length play = 7 then turnScore <- turnScore + 50;
			isWord (this#stripLetters !wrd) && !perp && validPos)
		else false)


	method refresh () = 
		passes <- 0;
		dumps <- [];
	    dumping <- false;
		scores.(turn) <- scores.(turn) + turnScore;
		turnScore <- 0;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- w2) w2s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- l2) l2s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- w3) w3s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- l3) l3s;
		List.iter (fun (x, y) -> layout.(x).(y)#unclick; layout.(x).(y)#setWordMult 1;
		 layout.(x).(y)#setLetterMult 1 ) play;
		play <- [];
		validPos <- false;
		let blanks = ref 0 in 
		for i = 0 to 6 do 
			if hands.(turn).(i)#isBlank then hands.(turn).(i) <- this#pullTile ();
			if hands.(turn).(i)#isBlank then blanks := !blanks + 1;
		done;
		if !blanks = 7 then this#endGame turn;

	method endScore (winner: int) =
		let playerMinus = ref 0 in 
		let winnerBonus = ref 0 in 
		for i = 0 to players - 1 do
			for j = 0 to 6 do
				playerMinus := !playerMinus + hands.(i).(j)#getscore
			done;
			winnerBonus := !winnerBonus + !playerMinus;
			scores.(i) <- scores.(i) - !playerMinus;
			playerMinus := 0;
		done;
		scores.(winner) <- scores.(winner) + !winnerBonus

	method endGame (winner: int) = 
		this#endScore winner;
		Graphics.draw_string ("PLAYER " ^ (string_of_int winner) ^ " WINS!");



	method findWinner () = 
		let runningMax = ref 0 in
		let maxPos = ref 0 in
		for i = 0 to players - 1 do
			if scores.(i) > !runningMax then (runningMax := scores.(i); maxPos := i)
		done;
		!maxPos

	
	method reset () = 
		dumping <- false;
		turnScore <- 0;
		let storage = ref [] in 
		List.iter (fun (x, y) -> storage := layout.(x).(y) :: !storage;
								 layout.(x).(y) <- blank) play;
		play <- [];
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- w2) w2s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- l2) l2s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- w3) w3s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- l3) l3s;
		validPos <- false;
		for i = 0 to 6 do 
			if hands.(turn).(i)#isBlank then 
				match !storage with
				|[] -> ();
				|hd :: tl -> hands.(turn).(i) <- hd; storage := tl;
			else ();
			hands.(turn).(i)#unclick;
			hands.(turn).(i)#setWordMult 1;
			hands.(turn).(i)#setLetterMult 1;
		done	

	method wait x = 
		let rec delay (sec: float) : unit =
  			try ignore(Thread.delay sec)
 			with Unix.Unix_error _ -> delay sec in 
 		delay x
	
	method keyParse k = 
		if k = 'h' then help <- not help;
		if k = ' ' then 
			if this#is_valid () then (this#refresh (); this#advanceTurn ();)
			else this#reset ()
		else if k = 'r' then this#reset ()
	    else if k = 'd' && dumping then 
	      (this#dump dumps ;
	      this#refresh () ;
	  	  this#advanceTurn ())
	    else if k = 'd' then dumping <- true
	 	else if k = 'p' then (
	 		passes <- passes + 1; 
	 		if passes = players then this#endGame (this#findWinner ())
	 		else (this#reset (); this#advanceTurn ());)
	 	else if k = 'x' then raise Exit
	 	

	method react (s: Graphics.status) = 
		if s.keypressed then this#keyParse s.key
		else this#mouseClick s.mouse_x s.mouse_y
		




	end 



	
