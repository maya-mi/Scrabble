open Words;;	

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 18;;

class tile  ({id; score}: letter) = 
 object (this)
 	val shadow = Graphics.rgb 222 184 135
 	val tile_color = Graphics.rgb 255 239 213 
 	val blank_color = Graphics.rgb 205 205 193
 	val lines = Graphics.rgb 139 137 112
 	val mutable ch = id
 	val mutable score = score
 	val mutable isClicked = false
 	val mutable x = min_int
 	val mutable y = min_int
 	val mutable wordMultiplier = 1
 	val mutable letterMultiplier = 1

 	method init {id; score = n} : unit = 
 		ch <- id;
 		score <- n

 	method getX = x
 	method getY = y

 	method isBlank = ch = ' '

 	method click = isClicked <- true
 	method unclick = isClicked <- false

 	method getLetter = {id = this#getid; score = this#getscore}

 	method getid  = id
 	method getscore = score

 	method setWordMult x = wordMultiplier <- x
 	method getWordMult = wordMultiplier

 	method draw x0 y0 = 
 		let edge = length / 8 in
 		x <- x0;
 		y <- y0;
 		let xc = (x + 1) * length in
 		let yc = (y + 2) * length in 
 		if this#isBlank then (
 			Graphics.set_color blank_color; 
 			if wordMultiplier = 2 then Graphics.set_color Graphics.red;
 			Graphics.fill_rect xc yc length length;
 			Graphics.set_color lines;
 			Graphics.draw_rect xc yc length length)
 		else(
 		let l = Array.of_list [(xc, yc); (xc + edge, yc + edge); (xc + edge, yc + length - edge); (xc, yc + length)] in 
 		let d = Array.of_list [(xc, yc); (xc + edge, yc + edge); (xc + length - edge, yc + edge); (xc + length, yc)] in 
 		let r = Array.of_list [(xc + length, yc); (xc + length - edge, yc + edge); (xc + length - edge, yc + length - edge); (xc + length, yc + length)] in 
 		let u = Array.of_list [(xc, yc + length); (xc + length, yc + length); (xc + length - edge, yc + length - edge); (xc + edge, yc + length - edge)] in 

 		Graphics.set_color tile_color;
 		Graphics.fill_rect xc yc length length;
 		Graphics.set_color shadow;
 		Graphics.fill_poly l;
 		Graphics.fill_poly d;

 		Graphics.draw_poly l;
 		Graphics.draw_poly d;
 		Graphics.draw_poly r;
 		Graphics.draw_poly u;
 		if isClicked then Graphics.set_color Graphics.red else Graphics.set_color Graphics.black;
 		Graphics.moveto (xc + length / 2) (yc + 3 * edge);
 		Graphics.draw_char ch;
 		Graphics.moveto (xc + 2 * length / 3) (yc + length / 6);
 		Graphics.draw_string (string_of_int score);)


						   
 end



