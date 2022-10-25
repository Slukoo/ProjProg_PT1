type nombre = Int of int | Float of float
type binaire = Add | Sub | Mul | Div | Mod | Addf | Subf | Mulf 
type unaire = Toint | Tofloat | Minus

type sexp = 
  | Number of nombre
  | Unary of unaire * sexp
	| Binary of binaire * sexp * sexp

let rec affiche= function
  | Unary (Toint,exp) ->
      print_string " i(";
      affiche exp;
      print_char ')'
  | Unary (Tofloat,exp) ->
      print_string " f(";
      affiche exp;
      print_char ')'
  | Unary (Minus, exp) ->
  		print_string "-( ";
  		affiche exp;
  		print_char '('
  | Number (Int ent) ->
      print_char '(';
      print_int ent;
      print_char ')'
  | Number (Float flo) ->
      print_char '(';
      print_float flo;
      print_char ')'
  | Binary (Add, s1, s2) ->
      print_string "(";
      affiche s1;
      print_string " + ";
      affiche s2;
      print_char ')'
  | Binary (Sub, s1, s2) ->
      print_string "(";
      affiche s1;
      print_string " - ";
      affiche s2;
      print_char ')'
  | Binary (Mul, s1, s2) ->
      print_string "(";
      affiche s1;
      print_string " * ";
      affiche s2;
      print_char ')'
  | Binary (Div, s1, s2) ->
      print_string "(";
      affiche s1;
      print_string " / ";
      affiche s2;
      print_char ')'
  | Binary (Mod, s1, s2) ->
      print_string "(";
      affiche s1;
      print_string " % ";
      affiche s2;
      print_char ')'
  | Binary (Addf, s1, s2) ->
      print_string "(";
      affiche s1;
      print_string " +. ";
      affiche s2;
      print_char ')'
  | Binary (Subf, s1, s2) ->
      print_string "(";
      affiche s1;
      print_string " -. ";
      affiche s2;
      print_char ')'
  | Binary (Mulf, s1, s2) ->
      print_string "(";
      affiche s1;
      print_string " *. ";
      affiche s2;
      print_char ')'
