type nombre = Int of int | Float of float
type binaire = Add | Sub | Mul | Div | Mod | Addf | Subf | Mulf 
type unaire = Toint | Tofloat | Minus


type ast = 
  | Number of nombre
  | Unary of unaire * ast
	| Binary of binaire * ast * ast
	
	
