open Words;;

class tile : letter -> object
	
	method isBlank : bool
	method getX : int
	method getY : int
	method getid : char
	method getscore: int
	method getLetter: letter 
	method print : unit
	method click : unit
	method unclick : unit

	method setProp: char -> int -> unit

	method setWordMult: int -> unit
	method getWordMult: int

	method setLetterMult: int -> unit
	method getLetterMult: int

	method draw : int -> int -> unit

 end