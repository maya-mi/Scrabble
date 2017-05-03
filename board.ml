open Tile;;
open Words;;
open PermHelp ;;
open Graphics;;

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 18;;

(*Create the blank tile we will use to populate the board*)
let blank = new tile {id = char_of_int 32; score = 0};;

(*FUNCTION USED THROUGHOUT THE BOARD CLASS:*)
let inRange x l m = 
	x >= l && x <= m;;

class board (players: int) (ais:int) = 
	object (this)

	(*The actual matrix that keeps track of our board*)
	val mutable layout = Array.make_matrix 15 15 blank
	(*A shuffled set of tiles-- see word.ml*)
	val mutable drawPile = shuffle fullSet
	(*Common color for the background*)
	val grn = rgb 84 139 84

	(*PLAYER VARIABLES*)
	(*Each entry .(n) corresponds to the quanitity for the n+1th player*)
	val mutable hands = Array.make_matrix players 7 blank
	val mutable reals = Array.make players false
	val mutable scores = Array.make players 0
	val mutable turn = 0
	(*Tracks consecutive passes*)
	val mutable passes = 0

	(*A saved position in the hand, assigned when toggleClick = false*)
	val mutable savedQ = min_int
	val mutable toggleClicked = false
	(*List that stores coordinates on which tiles have been played in a turn*)
	val mutable play = []
	(*Updated by validating tiles laid on the board*)
	val mutable validPos = false
	val mutable turnScore = 0	

	(*Hand positions to be dumped*)
	val mutable dumps = []
	val mutable dumping = false

	(*Show special screen for help*)
	val mutable help = false

	val mutable standL = [0; 1; 2; 3; 4; 5; 6; 7; 8]
	
	val perms = perm1 [0; 1; 2; 3; 4; 5; 6; 7; 8]
	(*Board coordinates for special tiles*)
	val w2s = [(1, 1); (2, 2); (3, 3); (4, 4); (10, 10); (11, 11); (12, 12); (13, 13);
			   (1, 13); (2, 12); (3, 11); (4, 10); (10, 4); (11, 3); (12, 2); (13, 1)]
    val l2s = [(0, 3); (0, 11); (3, 0); (11, 0); (14, 3); (14, 11); (3, 14); (11, 14);
    		   (6, 6); (8, 8); (8, 6); (6, 8);
    		   (2, 6); (2, 8); (3, 7); (6, 2); (8, 2); (7, 3);
    		   (12, 6); (12, 8); (11, 7); (6, 12); (8, 12); (7, 11)]
    val w3s = [(0, 0); (7, 0); (14, 0); (0, 7); (0, 14); (7, 14); (14, 7); (14, 14)]
    val l3s = [(5, 5); (1, 5); (1, 9); (5, 9); (5, 13); (9, 13); (9, 9); (13, 9); (13, 5); (9, 5); (9, 1); (5, 1)]
    		   
    (*Grabs the next tile from the shuffled deck. Blank if deck has been exhausted*)
	method pullTile () = 
		match drawPile with
		|[] -> blank
		|hd :: tl -> drawPile <- tl; new tile hd

	(*GRAPHICS: DRAWING THE BOARD*)
	method drawSetting () = 
		set_color grn;
		fill_rect 0 0 cFRAMESIZE cFRAMESIZE;
		set_color black;
		Array.iteri (fun index x -> 
		moveto (length * index * 7 / 2 + length) (cFRAMESIZE - length);
		draw_string ("PLAYER " ^ (string_of_int (index + 1)) ^ "'S SCORE: " ^ (string_of_int x))) scores ;
		moveto (cFRAMESIZE - 3 * length) (cFRAMESIZE - length);
		let msg = "PLAYER " ^ (string_of_int (1 + turn)) ^ "'S TURN" in 
		let (x, y) = text_size msg in 
		draw_string msg; 
		set_color red;
		draw_rect (cFRAMESIZE - 3 * length - 5) (cFRAMESIZE - length - 5) (x + 10) (y + 10)


    (*A special screen we enter when 'h' is pressed*)
	method drawHelp () = 
		let w2 = (new tile {id = char_of_int 32; score = 0}) in 
		w2#setWordMult 2;
		let l2 = (new tile {id = char_of_int 32; score = 0}) in 
		l2#setLetterMult 2;
		let w3 = (new tile {id = char_of_int 32; score = 0}) in
		w3#setWordMult 3;
		let l3 = (new tile {id = char_of_int 32; score = 0}) in
		l3#setLetterMult 3;
		this#drawSetting ();
		l2#draw 1 3;
		l3#draw 1 5;
		w2#draw 1 8;
		w3#draw 1 10;
		set_color black;
		moveto (cFRAMESIZE / 5) (length * 25 / 2);
		draw_string "Triple word score tile";
		moveto (cFRAMESIZE / 5) (length * 21 / 2);
		draw_string "Double word score tile";
		moveto (cFRAMESIZE / 5) (length * 15 / 2);
		draw_string "Triple letter score tile";
		moveto (cFRAMESIZE / 5) (length * 11 / 2);
		draw_string "Double letter score tile";
		let msgs = ["'p' passes"; 
					"Enter 'd' to select tiles to dump, and 'd' to finalize dump";
					"'r' resets the board, including unfinished dumps"; "Hit the space bar to score a word"; "'x' exits the game"; "PRESS 'h' TO EXIT HELP"] in 
		List.iteri (fun i msg -> moveto (cFRAMESIZE / 2) (cFRAMESIZE - length * (i + 7)); draw_string msg) msgs

	method drawBoard () = 
		this#drawSetting();
		for i = 0 to 14 do
		  for j = 0 to 14 do 
		    layout.(i).(j)#draw i j;
		  done
		done;

	method drawHand () = 
		for i = 0 to 6 do
			hands.(turn).(i)#draw 16 (i + 4);
		done

	method restoreSpecials () = 
		let w2 = (new tile {id = char_of_int 32; score = 0}) in
		w2#setWordMult 2;
		let l2 = (new tile {id = char_of_int 32; score = 0}) in
		l2#setLetterMult 2;
		let w3 = (new tile {id = char_of_int 32; score = 0}) in 
		w3#setWordMult 3;
		let l3 = (new tile {id = char_of_int 32; score = 0}) in
		l3#setLetterMult 3;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- w2) w2s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- l2) l2s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- w3) w3s;
		List.iter (fun (x, y) -> if layout.(x).(y)#isBlank then layout.(x).(y) <- l3) l3s;

	method draw () = 
		if help then this#drawHelp ()
		else
			(this#drawBoard ();
			this#drawHand ())

	(*Initializes the board, including special tiles. Draws players hands,
	informs which are AI*)
	method init () = 
		this#restoreSpecials ();
		for i = 0 to players - 1 do
			for j = 0 to 6 do 
				hands.(i).(j) <- this#pullTile ();
			done;
		done;
		for i = 0 to players - ais - 1 do 
			reals.(i) <- true;
		done;


	(*END OF GAME METHODS*)	

	(*Finds the player with the highest score*)
	method findWinner () = 
		let runningMax = ref 0 in
		let maxPos = ref 0 in
		for i = 0 to players - 1 do
			if scores.(i) > !runningMax then (runningMax := scores.(i); maxPos := i)
		done;
		!maxPos

	(*Performs standard Scrabble scoring at the end of the game; if a player
	empties their rack, they get the combined point total of the tiles left in the
	other players' hands. Every other player loses the point total of their hand*)
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

	(*Displays winner message, waits, exits the game*)
	method endGame (winner: int) = 
		this#draw ();
		let rec delay (sec: float) : unit =
  			try ignore(Thread.delay sec)
 			with Unix.Unix_error _ -> delay sec in 
		let msg = "PLAYER " ^ (string_of_int (winner + 1)) ^ " WINS!" in
		moveto (cFRAMESIZE - 2 * length - 5) (cFRAMESIZE - 2 * length);
		set_color black;
		draw_string msg;
		delay 5.;
		raise Exit
	

	(*After a dump is finalized, reshuffles deck including dumped letters
	and replaces dumped indices*)
	method dump () =
	  drawPile <- List.fold_left (fun acc q -> hands.(turn).(q)#getLetter :: acc) drawPile dumps;
	  drawPile <- shuffle drawPile;
	  List.iter (fun i -> hands.(turn).(i) <- this#pullTile ()) dumps;
	  this#reset ();


	(*AI FUNCTIONS*)
    (* breaks a list into those elemnts which came before and those which came after break point *)
    method break (point : int) (input : int list) (acc : int list) : (int list) * (int list) =
      match input with
      | h :: t -> if h = point then (acc, t) else this#break point t (h :: acc)
      | [] -> failwith "False case"

    (* cuts off all elements past and at 7 in a list *)
    method cutPost7 (somePerm : int list) (acc : int list) =
      match somePerm with
      | h :: t -> if h = 7 then acc else this#cutPost7 t (acc @ [h])
      | [] -> failwith "unexpected 7 behavior"


    (* accumulates a word by moving along given velocity until off board or blank *)
    method makeSide (xv, yv) (x, y) acc =
      if x >= 0 && x <= 14 && y >= 0 && y >= 14 then begin
        if layout.(x).(y)#isBlank then acc
        else begin
      	  if xv = 1 then this#makeSide (xv, yv) (xv + x, yv + y) (acc @ [layout.(x).(y)])
          else if xv = - 1 then this#makeSide (xv, yv) (xv + x, yv + y) (layout.(x).(y) :: acc)
          else if yv = -1 then this#makeSide (xv, yv) (xv + x, yv + y) (acc @ [layout.(x).(y)])
          else this#makeSide (xv, yv) (xv + x, yv + y) (layout.(x).(y) :: acc);
        end
      end
      else acc 


   (* determines the move for the ai to make *)
	method playAI () =
	  this#draw ();
	  moveto (cFRAMESIZE - 2 * length) (cFRAMESIZE - 2 * length);
	  draw_string "AI THINKING";
	  let prePass = passes in
	  (* saves the best score and corresponding play over all searches*)
      let bScore = ref 0 in
      let bPlay = ref [] in
      let tryMove (order : int list) isHorizontal : unit =
        (* word accumulated along main axis *)
        let curWord = ref [] in
        let sideWord = ref [] in
        let curPlay = ref [] in
        (* cuts off permutation at 7, determines length of play *)
        let newOrder = this#cutPost7 order [] in
        (* 8 is break point determining how much goes before, after tile *)
        (* doesn't make sense to play if there is no break point (ie it came after 7) *)
        if List.find_all (fun alpha -> alpha = 8) newOrder = [] then () else begin
          let pre, post = this#break 8 newOrder [] in
          (* must play on cur square via pre *)
          if List.length pre = 0 then () else begin
            (* recursively lays down tiles and adds tiles along main axis into curWord *)
            let rec placeTiles ((xv, yv) : int * int) ((x1, y1) : int*int) (indexes : int list) : unit =
              match indexes with
              | [] -> 
                if x1 >= 0 && x1 <= 14 && y1 >= 0 && y1 <= 14 then begin
                  (* if not blank, we want add letter into our word *)
                  if not layout.(x1).(y1)#isBlank then begin
                    if xv = 1 then begin curWord := !curWord @ [layout.(x1).(y1)]; placeTiles (xv, yv) (x1 + xv, y1 + yv) [] end
                    else if xv = -1 then begin curWord := layout.(x1).(y1) :: !curWord; placeTiles (xv, yv) (x1 + xv, y1 + yv) [] end
                    else if yv = 1 then begin curWord := layout.(x1).(y1) :: !curWord; placeTiles (xv, yv) (x1 + xv, y1 + yv) [] end
                    else begin curWord := !curWord @ [layout.(x1).(y1)]; placeTiles (xv, yv) (x1 + xv, y1 + yv) [] end
                  end
                end
                else ()
              | h :: t -> 
                if x1 >= 0 && x1 <= 14 && y1 >= 0 && y1 <= 14 then begin
                  (* if no tile here then we want to remember to lay tile down there later *)
                  if layout.(x1).(y1)#isBlank then begin
          	        curPlay := (h, hands.(turn).(h), (x1, y1)) :: !curPlay;
                    if xv = 1 then begin curWord := !curWord @ [hands.(turn).(h)]; placeTiles (xv, yv) (x1 + xv, y1 + yv) t end
                    else if xv = -1 then begin curWord := hands.(turn).(h) :: !curWord; placeTiles (xv, yv) (x1 + xv, y1 + yv) t end
                    else if yv = 1 then begin curWord := hands.(turn).(h) :: !curWord; placeTiles (xv, yv) (x1 + xv, y1 + yv) t end
                    else begin curWord := !curWord @ [hands.(turn).(h)]; placeTiles (xv, yv) (x1 + xv, y1 + yv) t end
                  end
                  else begin
                    if xv = 1 then begin curWord := !curWord @ [layout.(x1).(y1)]; placeTiles (xv, yv) (x1 + xv, y1 + yv) indexes end
                    else if xv = -1 then begin curWord := layout.(x1).(y1) :: !curWord; placeTiles (xv, yv) (x1 + xv, y1 + yv) indexes end
                    else if yv = 1 then begin curWord := layout.(x1).(y1) :: !curWord; placeTiles (xv, yv) (x1 + xv, y1 + yv) indexes end
                    else begin curWord := !curWord @ [layout.(x1).(y1)]; placeTiles (xv, yv) (x1 + xv, y1 + yv) indexes end
                  end
                end
            in
            for x = 0 to 14 do
              for y = 0 to 14 do
                curWord := [];
                curPlay := [];
                sideWord := [];
                (* validating if adjacent to played tile *)
                if (this#validating x y && layout.(x).(y)#isBlank) then begin
                  (if isHorizontal then begin
                  	(* places tile after start with post, positive x velocity *)
                  	(* also accumulating perpendicular word *)
                    placeTiles (1, 0) (x + 1, y) post;
                    sideWord := this#makeSide (0, 1) (x, y) [];
                    (* pre, negative x velocity *)
                    placeTiles (-1, 0) (x, y) pre;
                    sideWord := this#makeSide (0, -1) (x, y - 1) !sideWord;
                  end
                  else begin
                  	(* same, except in the y *)
              	    placeTiles (0, -1) (x, y) pre;
              	    sideWord := this#makeSide (1, 0) (x, y) [];
              	    placeTiles (0, 1) (x, y + 1) post;
              	    sideWord := this#makeSide (-1, 0) (x - 1, y) !sideWord;
                  end);
                  (* a bare minimum is that word along main axis must be word, 
                  and that word perpendicualr from main tile is *)
                  if isWord (this#stripLetters !curWord) && 
                  (isWord (this#stripLetters !curWord) || List.length !curWord < 2) then begin
                  	print (this#stripLetters !sideWord);
                  	(* lays stored tiles onto board, adds relevent squares to play *)
				    List.iter (fun (h, t, (x1, y1)) ->
				      this#inheritscoring x y t;
				      layout.(x1).(y1) <- hands.(turn).(h);
					  play <- (x1, y1) :: play;
					  ) !curPlay;
                    validPos <- true;
                    (* uses general scoring function *)
                    if this#is_valid () then begin
                      if turnScore > !bScore then begin bPlay := !curPlay; bScore := turnScore; end
                    end;
                    List.iter (fun (_, t, (x1, y1)) -> layout.(x1).(y1)<- blank; 
                      t#setLetterMult 1; t#setWordMult 1) !curPlay;
                    this#restoreSpecials ();
                    this#reset ();
                  end
                end
              done;
            done;
          end
        end
        in
        (* tries 20000 different random permutations *)
        for _i = 0 to 20000 do
          standL <- shuffle standL;
          tryMove standL true;
          tryMove standL false;
        done;
        (* resets board state *)
        this#reset ();
        (* passes inadvertenly changed by reset, so recall it *)
        passes <- prePass;
        validPos <- true;
        (* iterating through, setting mult on tile to proper one when we place it *)
        (* also blanks tile in hand, so on reset new tiles are drawn from bag *)
        List.iter (fun (h, t, (x1, y1)) -> this#inheritscoring x1 y1 t;
          layout.(x1).(y1) <- t; play <- (x1, y1) :: play; hands.(turn).(h) <- blank) !bPlay;
        (* we have no play, so must pass *)
        if List.length play = 0 then this#pass ()
        else begin
          (if this#is_valid () then () else failwith "unexpected behavior");
          this#refresh ()
        end


    method inheritscoring x y t = 
      let wm = layout.(x).(y)#getWordMult in
      t#setWordMult wm;
      let lm = layout.(x).(y)#getLetterMult in
      t#setLetterMult lm


	(*A square is validating if it has at least one live neighbor; a single 
	validating square makes the placement of the entire word valid. *)
	method validating x y = 
		let n = ref [] in 
		if x - 1 > -1 then (n := layout.(x - 1).(y) :: !n);
		if y + 1 < 15 then (n := layout.(x).(y + 1) :: !n);
		if x + 1 < 15 then (n := layout.(x + 1).(y) :: !n);
		if y - 1 > -1 then (n := layout.(x).(y - 1) :: !n);
		let playedTiles = List.map (fun (x, y) -> layout.(x).(y)) play in  
		(x = 7 && y = 7) || List.length (List.filter 
			(fun x -> not (List.mem x playedTiles || x#isBlank)) !n) > 0

	(*Looks for 3 behaviors: dumping, playing an already clicked tile on the
	board, or selecting a tile from the hand to play on the board*)
	method mouseClick mouse_x mouse_y= 
		(*Dumping: adds indices to dump*)
		if dumping then
			(let q = mouse_y / length - 6 in 
			if (inRange q 0 6 && inRange mouse_x (cFRAMESIZE - length) cFRAMESIZE && not (List.mem q dumps)) then 
			 		dumps <- q :: dumps;
			    	hands.(turn).(q)#click) 
		else (
			(*Playing to board; gets coordinates, picks up multipliers from
			the board pos, records play coordinate in play*)
			if toggleClicked then 
				let x = mouse_x / length - 1 in
				let y = mouse_y / length - 2 in
				if (inRange x 0 14 && inRange y 0 14 && layout.(x).(y)#isBlank) then 
					(let wm = layout.(x).(y)#getWordMult in 
					hands.(turn).(savedQ)#setWordMult wm;
					let lm = layout.(x).(y)#getLetterMult in 
					hands.(turn).(savedQ)#setLetterMult lm;
					layout.(x).(y) <- hands.(turn).(savedQ);
					hands.(turn).(savedQ) <- blank;
					play <- (x, y):: play;
					validPos <- validPos || this#validating x y) 
				else (for i = 0 to 6 do
						hands.(turn).(i)#unclick;
					done);
				toggleClicked <- false
			(*Selecting tile from hand: index stored in savedQ, toggleClick*)
			else 
				let q = mouse_y / length - 6 in 
				if (inRange q 0 6 && inRange mouse_x (cFRAMESIZE - length) cFRAMESIZE 
					&& not hands.(turn).(q)#isBlank) then 
			 		(hands.(turn).(q)#click; savedQ <- q; toggleClicked <- true))


	(*CHECKING VALIDITY OF A PLAY*)

	(*Given the bounds of a vertical play, checks for live neighbor tiles to 
	either side of the played tiles*)
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
		
	(*Given the bounds of a horizontal play, checks for live neighbor tiles to 
	either side of the played tiles*)
	method addHor wrd y xMax xMin: unit = 
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
	
	(*Checks if vertical tangent tiles are valid at a given coordinate.
	Augments turnScore*)
	method vertNormal x y: bool = 
		let wrd = ref [layout.(x).(y)] in 
		this#addVerts wrd x y y;
		if isWord (this#stripLetters !wrd) then 
			(turnScore <- turnScore + (this#score !wrd); true)
		else List.length !wrd = 1 

	(*Checks if horizontal tangent tiles are valid at a given coordinate.
	Augments turnScore*)
	method horNormal x y: bool = 
		let wrd = ref [layout.(x).(y)] in 
		this#addHor wrd y x x;
		if isWord (this#stripLetters !wrd) then 
			(turnScore <- turnScore + (this#score !wrd); true)
		else List.length !wrd = 1 
	
	(*Scores a list of tiles, taking multipliers into account*)	
	method score (tls: tile list) = 
	 let rec help tls scr mult = 
		match tls with 
		|[] -> scr * mult
		|hd::tl -> help tl (scr + hd#getscore) (mult * hd#getWordMult) in 
	help tls 0 1

	(*Creates a word, or letter list, out of a tile list*)
	method stripLetters (tls: tile list): word = 
	 List.map (fun t -> t#getLetter) tls
	

	(*Checks if a play (including tangents) is valid, updating turnScore*)
	method is_valid () = 
	  (*Finds the extrema of a list-- max or min passed in as f*)
	  let listFind f lst = 
		match lst with
		|hd :: tl -> List.fold_left f hd tl
		|[] -> failwith "Empty" in 
	  (*Examines the x,y values of coordinate list to find lines, returns a
	  bool tuple*)
	  let compare_all tup_lst = 
			let compare lst =
				match lst with
				|[] -> true
				|hd :: tl -> List.fold_left (fun acc x -> acc && x = hd) true tl in 
	  let xs, ys = List.split tup_lst in
	  (compare xs, compare ys) in
	  let xs, ys = List.split play in
	  match xs, ys with 
	  	|[], _ 
	  	|_, [] -> false
	  	|h1:: _, h2::_ -> let x, y = h1, h2 in 
	  (*Handles the one-letter case; as it is not obvious which is the primary
	  direction, checks for tangent words in both*)
	  if List.length play = 1 then 
		  let _ = this#vertNormal x y in
		  let _ = this#horNormal x y in 
		  turnScore > 0
	  else( 
		let xSame, ySame = compare_all play in
		(*Vertical case*)
		if xSame then 
			(let wrd = ref [] in 
			let perp = ref true in 
			for i = (listFind min ys) to (listFind max ys) do
				wrd := layout.(x).(i) :: !wrd;
				if List.mem (x, i) play then 
					(perp := !perp && this#horNormal x i;)
			done;
			this#addVerts wrd x (listFind max ys) (listFind min ys);
			turnScore <- turnScore + (this#score !wrd);
			if List.length play = 7 then turnScore <- turnScore + 50;

			isWord (this#stripLetters !wrd) && !perp && validPos)
		(*Horizontal case*)
		else if ySame then 
			(let wrd = ref [] in 
			let perp = ref true in 
			for i = (listFind min xs) to (listFind max xs) do
				wrd := !wrd @ [layout.(i).(y)];
				if List.mem (i, y) play then 
					(perp := !perp && this#vertNormal i y;)
			done;
			this#addHor wrd y (listFind max xs) (listFind min xs);
			turnScore <- turnScore + (this#score !wrd);
			if List.length play = 7 then turnScore <- turnScore + 50;
			isWord (this#stripLetters !wrd) && !perp && validPos)
		else false)



	(*Called after a valid play; readies board for next turn*)
	method refresh () = 
		passes <- 0;
		dumps <- [];
	    dumping <- false;
		scores.(turn) <- scores.(turn) + turnScore;
		turnScore <- 0;
		this#restoreSpecials ();
		List.iter (fun (x, y) -> layout.(x).(y)#unclick; layout.(x).(y)#setWordMult 1;
		 layout.(x).(y)#setLetterMult 1 ) play;
		play <- [];
		validPos <- false;
		(*Checks if a player's hand is blank after attempting to refill*)
		let blanks = ref 0 in 
		for i = 0 to 6 do 
			if hands.(turn).(i)#isBlank then hands.(turn).(i) <- this#pullTile ();
			if hands.(turn).(i)#isBlank then blanks := !blanks + 1;
		done;
		if !blanks = 7 then 
			(this#endScore turn;
			this#endGame (this#findWinner ()))
	
	(*Called after an invalid play; cleans up board for next attempt*)
	method reset () = 
		dumping <- false;
		turnScore <- 0;
		let storage = ref [] in 
		List.iter (fun (x, y) -> storage := layout.(x).(y) :: !storage;
								 layout.(x).(y) <- blank) play;
		play <- [];
		this#restoreSpecials ();
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
			this#restoreSpecials ();
		done	


	(*Advances instance variable turn to reflect next turn. Plays AI if appt*)
	method advanceTurn () = 
	 let rec help () = 
		turn <- (turn + 1) mod (players);
		if not reals.(turn) then (this#playAI (); help ())
     in 
	 help ()


	method pass () = 
	    passes <- passes + 1;
	 	if passes = players then 
	 		(this#reset (); this#endGame (this#findWinner ()))
	 	else this#reset ()


	method keyParse k = 
		if k = 'h' then help <- not help;
		if k = ' ' then 
			if this#is_valid () then (this#refresh (); this#advanceTurn ();)
			else this#reset ()
		else if k = 'r' then this#reset ()
	    else if k = 'd' && dumping then 
	      (this#dump ();
	      this#refresh ();
	  	  this#advanceTurn ())
	    else if k = 'd' then dumping <- true
	 	else if k = 'p' then (
	 		this#pass (); this#advanceTurn ())
	 	else if k = 'x' then raise Exit
	 	





	(*Responds to the graphics status passed in: delegates to mouseClick
	or keyParse as appropriate*)
	method react (s: Graphics.status) = 
		if s.keypressed then this#keyParse s.key
		else this#mouseClick s.mouse_x s.mouse_y
		



	end ;;


	
