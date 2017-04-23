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

class board = 
	object (this)

	val mutable layout = Array.make_matrix 15 15 blank
	val mutable drawPile = shuffle fullSet
	val mutable hand1 = Array.make 7 blank
	val mutable savedQ = min_int
	val mutable toggleClicked = false
	val mutable play = []
	val mutable validPos = false
	val mutable score1 = 0
	val mutable turnScore = 0

	method pullTile () = 
		match drawPile with
		|[] -> new tile {id = char_of_int 32; score = 0}
		|hd :: tl -> drawPile <- tl; new tile hd

	method drawBoard () = 
		for i = 0 to 14 do
			for j = 0 to 14 do 
				layout.(i).(j)#draw i j;
			done
		done;
		Graphics.set_color Graphics.red;
		layout.(7).(7)#draw 7 7; 
		Graphics.set_color Graphics.black;

	method drawHand () = 
		for i = 0 to 6 do
			hand1.(i)#draw 16 (i + 4);
		done


	method draw () = 
		Graphics.moveto (cFRAMESIZE - 2 * length) (cFRAMESIZE - length);
		Graphics.draw_string ("Score: " ^ (string_of_int score1));
		this#drawBoard ();
		this#drawHand ()

	method init () = 
		for i = 0 to 6 do 
			hand1.(i) <- this#pullTile ();
		done

	method validating x y = 
		let n = ref [] in 
		if x - 1 > -1 then (n := layout.(x - 1).(y) :: !n);
		if y + 1 < 15 then (n := layout.(x).(y + 1) :: !n);
		if x + 1 < 15 then (n := layout.(x + 1).(y) :: !n);
		if y - 1 > -1 then (n := layout.(x).(y - 1) :: !n);
		let playedTiles = List.map (fun (x, y) -> layout.(x).(y)) play in  
		(x = 7 && y = 7) || List.length (List.filter (fun x -> not (List.mem x playedTiles || x#isBlank)) !n) > 0 

	method mouseClick mouse_x mouse_y= 
		if toggleClicked then 
			let x = mouse_x / length - 1 in
			let y = mouse_y / length - 2 in
			if (inRange x 0 14 && inRange y 0 14) then 
				(layout.(x).(y) <- hand1.(savedQ);
				hand1.(savedQ) <- blank;
				play <- (x, y):: play;
				validPos <- validPos || this#validating x y) else ();
			toggleClicked <- false
		else 
			let q = mouse_y / length - 6 in 
			if (inRange q 0 7 && inRange mouse_x (cFRAMESIZE - length) cFRAMESIZE) then 
			 (savedQ <- q; toggleClicked <- true)

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
		score1 <- score1 + turnScore;
		turnScore <- 0;
		play <- [];
		validPos <- false;
		for i = 0 to 6 do 
			if hand1.(i)#isBlank then hand1.(i) <- this#pullTile ();
		done

	method reset () = 
		turnScore <- 0;
		let storage = ref [] in 
		List.iter (fun (x, y) -> storage := layout.(x).(y) :: !storage;
								 layout.(x).(y) <- blank) play;
		play <- [];
		validPos <- false;
		for i = 0 to 6 do 
			if hand1.(i)#isBlank then 
				match !storage with
				|[] -> failwith "freakout"
				|hd :: tl -> hand1.(i) <- hd; storage := tl;
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
			if this#is_valid () then this#refresh ()
			else this#reset ()
		else if k = 'r' then this#reset ()

	method react (s: Graphics.status) = 
		if s.keypressed then this#keyParse s.key
		else this#mouseClick s.mouse_x s.mouse_y
		




	end 



	
