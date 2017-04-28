open Tile;;
open Words;;

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 18;;
let blank = new tile {id = char_of_int 32; score = 0};;

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
	val mutable validPos = false
	val mutable scores = Array.make players 0
	val mutable turn = 0
	val mutable turnScore = 0	
	val mutable dumping = false
	val mutable dumps = []

	method pullTile () = 
		match drawPile with
		|[] -> new tile {id = char_of_int 32; score = 0}
		|hd :: tl -> drawPile <- tl; new tile hd

	method drawSetting () = 
		Graphics.set_color (Graphics.rgb 84 139 84);
		Graphics.fill_rect 0 0 cFRAMESIZE cFRAMESIZE;
		Graphics.set_color (Graphics.black);
		Graphics.moveto (cFRAMESIZE - 2 * length) (cFRAMESIZE - length);
		Graphics.draw_string ("SCORE: " ^ (string_of_int scores.(turn)));
		Graphics.moveto (cFRAMESIZE - 3 * length) (cFRAMESIZE - length/2);
		Graphics.draw_string ("PLAYER " ^ (string_of_int (1 + turn)) ^ "'S TURN");

	
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
		this#drawBoard ();
		this#drawHand ()

	method init () = 
		for i = 0 to players - 1 do
			for j = 0 to 6 do 
				hands.(i).(j) <- this#pullTile ();
			done;
		done;
		for i = 0 to players - ais - 1 do 
			reals.(i) <- true;
		done;

	method playAI () = ()

	method advanceTurn () = 
	 let rec help () = 
		turn <- (turn + 1) mod (players);
		if not reals.(turn) then (this#playAI (); help ()); in 
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
				(layout.(x).(y) <- hands.(turn).(savedQ);
				hands.(turn).(savedQ) <- blank;
				play <- (x, y):: play;
				validPos <- validPos || this#validating x y) else ();
			toggleClicked <- false
		else 
			let q = mouse_y / length - 6 in 
			if (inRange q 0 6 && inRange mouse_x (cFRAMESIZE - length) cFRAMESIZE) then 
			 (hands.(turn).(q)#click; savedQ <- q; toggleClicked <- true))

	method addVerts wrd x yMax yMin = 
		let ypos = ref yMax in  
		while !ypos < 14 && not layout.(x).(!ypos + 1)#isBlank do
			wrd := layout.(x).(!ypos + 1)#getLetter :: !wrd;
			ypos := !ypos + 1;
		done;
		ypos := yMin;
		while !ypos > 0 && not layout.(x).(!ypos - 1)#isBlank do
			wrd := !wrd @ [layout.(x).(!ypos - 1)#getLetter];
			ypos := !ypos - 1;
		done;
		

	method addHor wrd y xMax xMin = 
		let xpos = ref xMax in  
		while !xpos < 14 && not layout.(!xpos + 1).(y)#isBlank do
			wrd := !wrd @ [layout.(!xpos + 1).(y)#getLetter];
			xpos := !xpos + 1;
		done;
		xpos := xMin;
		while !xpos > 0 && not layout.(!xpos - 1).(y)#isBlank do
			wrd := layout.(!xpos - 1).(y)#getLetter :: !wrd;
			xpos := !xpos - 1;
		done;
	
	method vertNormal x y = 
		let wrd = ref [layout.(x).(y)#getLetter] in 
		this#addVerts wrd x y y;
		if isWord !wrd then (turnScore <- turnScore + (getScore !wrd); true)
		else List.length !wrd = 1 

	method horNormal x y = 
		let wrd = ref [layout.(x).(y)#getLetter] in 
		this#addHor wrd y x x;
		if isWord !wrd then (turnScore <- turnScore + (getScore !wrd); true)
		else List.length !wrd = 1 


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
				wrd := layout.(List.hd xs).(i)#getLetter :: !wrd;
				if List.mem ((List.hd xs), i) play then 
					(perp := !perp && this#horNormal (List.hd xs) i;)
			done;
			this#addVerts wrd (List.hd xs) (listFind max ys) (listFind min ys);
			turnScore <- turnScore + (getScore !wrd);
			isWord !wrd && !perp && validPos)
		else if ySame then 
			(let wrd = ref [] in 
			let perp = ref true in 
			for i = (listFind min xs) to (listFind max xs) do
				wrd := !wrd @ [layout.(i).(List.hd ys)#getLetter];
				if List.mem (i, (List.hd ys)) play then 
					(perp := !perp && this#vertNormal i (List.hd ys);)
			done;
			this#addHor wrd (List.hd ys) (listFind max xs) (listFind min xs);
			turnScore <- turnScore + (getScore !wrd);
			isWord !wrd && !perp && validPos)
		else false)


	method refresh () = 
		 dumps <- [] ;
	     dumping <- false;
		scores.(turn) <- scores.(turn) + turnScore;
		turnScore <- 0;
		List.iter (fun (x, y) -> layout.(x).(y)#unclick) play;
		play <- [];
		validPos <- false;
		for i = 0 to 6 do 
			if hands.(turn).(i)#isBlank then hands.(turn).(i) <- this#pullTile ();
		done

	method reset () = 
		dumping <- false;
		turnScore <- 0;
		let storage = ref [] in 
		List.iter (fun (x, y) -> storage := layout.(x).(y) :: !storage;
								 layout.(x).(y) <- blank) play;
		play <- [];
		validPos <- false;
		for i = 0 to 6 do 
			if hands.(turn).(i)#isBlank then 
				match !storage with
				|[] -> failwith "freakout"
				|hd :: tl -> hands.(turn).(i) <- hd; storage := tl;
			else ();
			hands.(turn).(i)#unclick;
		done
(*
	method reset () = 
		for i = 0 to 6 do 
			if hand1.(i)#isBlank then 
				match play with
				|[] -> failwith "freakout"
				|(x, y) :: tl -> hand1.(i) <- layout.(x).(y); play <- tl @ [(x, y)];
		done;
		List.iter (fun (x, y) -> layout.(x).(y) <- blank) play;
*)		
		
	
	method keyParse k = 
		if k = ' ' then 
			if this#is_valid () then (this#refresh (); this#advanceTurn ();)
			else this#reset ()
		else if k = 'r' then this#reset ()
	    else if k = 'd' && dumping then 
	      (this#dump dumps ;
	      this#refresh ();
	  	  this#advanceTurn ())
	    else if k = 'd' then dumping <- true

	method react (s: Graphics.status) = 
		if s.keypressed then this#keyParse s.key
		else this#mouseClick s.mouse_x s.mouse_y
		




	end 



	
