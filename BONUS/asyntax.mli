type nombre = Int of int | Float of float
type binaire = Add | Sub | Mul | Div | Mod | Addf | Subf | Mulf | Pow
type unaire = Toint | Tofloat | Minus | Fact


type ast = 
  | Number of nombre
  | Unary of unaire * ast
	| Binary of binaire * ast * ast
	
