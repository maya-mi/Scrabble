open Words;;	

let cFRAMESIZE = 750;;
let length = cFRAMESIZE / 18;;

class tile  ({id; score}: letter) = 
 object (this)
 	val mutable ch = id
 	val mutable score = score
 	val mutable x = min_int
 	val mutable y = min_int

 	method init {id; score = n} : unit = 
 		ch <- id;
 		score <- n

 	method getX = x
 	method getY = y

 	method isBlank = ch = ' '

 	method getLetter = {id = this#getid; score = this#getscore}

 	method getid  = id
 	method getscore = score


 	method draw x0 y0 = 
 		let l = length * 3 / 4 in
 		let edge = (length  - l) / 2 in
 		x <- x0;
 		y <- y0;
 		Graphics.draw_rect ((x + 1) * length + edge) ((y + 2) * length + edge) l l;
 		Graphics.moveto ((x + 1) * length + length / 2) ((y+ 2) * length + length / 2);
 		Graphics.draw_char ch
 		(*Graphics.draw_poly (Array.of_list [((x + 1) * length, (y + 1) * length);
 						   ((x + 1) * length + edge, (y + 1) * length) + edge);
 						   ((x + 1) * length + edge, (y + 2) * length) - edge);
						   ((x + 1) * length, (y + 2) * length) - edge)])*)
						   
 end



