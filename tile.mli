open Words;;

class tile : letter -> object
	
	method isBlank : bool
	method getX : int
	method getY : int
	method getid : char
	method getscore: int
	method getLetter: letter 


	method draw : int -> int -> unit
	method init: letter -> unit
 end