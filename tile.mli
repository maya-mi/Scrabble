open Words;;

class tile : letter -> object
	
	method isBlank : bool
	method getX : int
	method getY : int
	method getid : char
	method getscore: int
	method getLetter: letter 

	method click : unit
	method unclick : unit

	method setWordMult: int -> unit
	method getWordMult: int

	method setLetterMult: int -> unit
	method getLetterMult: int

	method draw : int -> int -> unit
	method init: letter -> unit
 end