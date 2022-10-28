type nombre = Int of int | Float of float
type binaire = Add | Sub | Mul | Div | Mod | Addf | Subf | Mulf | Pow
type unaire = Toint | Tofloat | Minus | Fact


type sexp = 
  | Number of nombre
  | Unary of unaire * sexp
	| Binary of binaire * sexp * sexp
	
