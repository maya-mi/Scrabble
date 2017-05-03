open Words;;	
open Graphics;;

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 18;;

class tile  ({id; score}: letter) = 
 object (this)
 	(*Colors used to draw tiles*)
 	val shadow = rgb 222 184 135
 	val tile_color = rgb 255 239 213 
 	val blank_color = rgb 205 205 193
 	val w2 = rgb 250 128 114
 	val w3 = rgb 178 34 34
 	val l2 = rgb 125 158 192
 	val l3 = rgb 0 104 139 
 	val lines = rgb 139 137 112

 	val mutable ch = id
 	val mutable score = score
 	val mutable isClicked = false
 	val mutable x = min_int
 	val mutable y = min_int
 	val mutable wordMultiplier = 1
 	val mutable letterMultiplier = 1


 	method getX = x
 	method getY = y

 	method setProp c s = ch <- c; score <- s



 	method print = print_char ch; print_endline ""

 	method isBlank = ch = ' '

 	(*Used in draw to visually indicate clicked tiles*)
 	method click = isClicked <- true
 	method unclick = isClicked <- false

 	method getLetter = {id = this#getid; score = this#getscore}

 	method getid  = id
 	method getscore = score * letterMultiplier

 	(*get and set special score multipliers*)
 	method setWordMult x = wordMultiplier <- x
 	method getWordMult = wordMultiplier
 	method setLetterMult x = letterMultiplier <- x
 	method getLetterMult = letterMultiplier

 	(*Uses Graphics methods to draw tile*)
 	method draw x0 y0 = 
 		let edge = length / 8 in
 		x <- x0;
 		y <- y0;
 		let xc = (x + 1) * length in
 		let yc = (y + 2) * length in 
 		(*Drawing a blank tile: if special score, fills with appt color*)
 		if this#isBlank then (
 			set_color blank_color; 
 			if x = 7 && y = 7 then set_color magenta;
 			if wordMultiplier = 2 then set_color w2;
 			if letterMultiplier = 2 then set_color l2; 
 			if wordMultiplier = 3 then set_color w3;
 			if letterMultiplier = 3 then set_color l3;
 			fill_rect xc yc length length;
 			set_color lines;
 			draw_rect xc yc length length)
 		(*Drawing a live tile:*)
 		else(
 		(*Trapezoid coord arrays for shading*)
 		let l = Array.of_list [(xc, yc); (xc + edge, yc + edge); 
 		  (xc + edge, yc + length - edge); (xc, yc + length)] in 
 		let d = Array.of_list [(xc, yc); (xc + edge, yc + edge); 
 		  (xc + length - edge, yc + edge); (xc + length, yc)] in 
 		let r = Array.of_list [(xc + length, yc); (xc + length - edge, yc + edge);
 		  (xc + length - edge, yc + length - edge); (xc + length, yc + length)] in
 		let u = Array.of_list [(xc, yc + length); (xc + length, yc + length);
 		  (xc + length - edge, yc + length - edge); (xc + edge, yc + length - edge)] in 

 		Graphics.set_color tile_color;
 		Graphics.fill_rect xc yc length length;
 		Graphics.set_color shadow;
 		Graphics.fill_poly l;
 		Graphics.fill_poly d;

 		Graphics.draw_poly l;
 		Graphics.draw_poly d;
 		Graphics.draw_poly r;
 		Graphics.draw_poly u;
 		if isClicked then Graphics.set_color Graphics.red 
 			else Graphics.set_color Graphics.black;
 		Graphics.moveto (xc + length / 2) (yc + 3 * edge);
 		Graphics.draw_char ch;
 		Graphics.moveto (xc + 2 * length / 3) (yc + length / 6);
 		Graphics.draw_string (string_of_int score);)


						   
 end



