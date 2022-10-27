open Format
open X86_64
open Asyntax




(*fonction pour écrire un code dans le fichier dont le nom est name*)
let write name code =
  let file = open_out name in
  let fmt = formatter_of_out_channel file in
  X86_64.print_program fmt code;
  close_out file;;

(*fonction qui donne le type final de l'expression (entier/flottant). on verifie au passage que l'arbre syntaxique est bien typé*)
let rec testype expr =
    match expr with
      | Number Int _ -> "int"
      | Number Float _ -> "float"
      | Unary (Toint, exp) -> if testype exp = "int" then failwith "mal typé" else "int"
      | Unary (Tofloat, exp) -> if testype exp = "float" then failwith "mal typé" else "float"
      | Unary (Minus, exp) -> testype exp
      | Binary (Add, exp1, exp2) | Binary (Sub, exp1, exp2) | Binary (Mul, exp1, exp2) | Binary (Div, exp1, exp2) | Binary (Mod, exp1, exp2) ->
          if ((testype exp1) = "int" && (testype exp2) = "int") then "int"
          else failwith "mal typé"
      | Binary (Addf, exp1, exp2) | Binary (Subf, exp1, exp2) | Binary (Mulf, exp1, exp2) ->
          if ((testype exp1) = "float" && (testype exp2) = "float") then "float"
          else failwith "mal typé" ;;
  




(*fonction qui donne le corps de notre code en fonction des opérations faites et du type final de l'expression (entier/flottant)*)
let main instructions typ = 
  if typ = "int" then           (*verison expression entière*)
  globl "main" ++ label "main" ++
            instructions ++
	          popq rdi ++             (*après tous les calculs de "instructions", on récupère le dernier élément dans notre pile pour l'imprimer*)
            call "print_int" ++
            ret ++
            inline "
  print_int:
      movq %rdi, %rsi
      movq $S_int, %rdi
      movq $0, %rax
      call printf
      ret
  "

  else                            (*version expression flottante*)
    globl "main" ++ label "main" ++
              instructions ++
              popf (reg xmm0)++
              call "print_float" ++
              ret ++
              inline "
  print_float:
	    movq $S_float, %rdi
	    movq $1, %rax
	    call printf
	    ret
";;

(*string nécéssaire à l'impression, qu'on choisit selon si notre expression est un entier ou un flottant*)
let printstr typ =
  if typ = "float" then label "S_float" ++ string "%f"
  else label "S_int" ++ string "%d";;


(*Fonctions d'opérations en assembleur*)


let store_int n =
    pushq (imm n);;
  
  
let store_float x i = 
    movfl (".FL" ^ (string_of_int i) ^ "(%rip)") (reg xmm0) ++
    pushf (reg xmm0)



let binary_int op =
	popq rdi ++
	popq rsi ++
	op (reg rdi) (reg rsi) ++
	pushq (reg rsi)

let binary_float op =
  popf (reg xmm0)++
  popf (reg xmm1)++
  op (reg xmm1) (reg xmm0) ++
  pushf (reg xmm0)

let divide =
	popq rdi ++
	popq rax ++
  inline "cqo\n" ++
	idivq (reg rdi) ++
	pushq (reg rax)

let modulo =
	popq rdi ++
	popq rax ++
  inline "cqo\n" ++
	idivq (reg rdi) ++
	pushq (reg rdx)


let minus_int =
  popq rdi ++
  movq (imm 0) (reg rsi) ++
  subq (reg rdi) (reg rsi) ++
  pushq (reg rsi)

let minus_float =
    popf (reg xmm0)++
    movfl ".ZERO(%rip)" (reg xmm1) ++
    subsd (reg xmm0) (reg xmm1) ++
    pushf (reg xmm1)


let toint =
    popf (reg xmm0)++
    cvttsd2si (reg xmm0) (reg rdi) ++
    pushq (reg rdi)

let tofloat =
    popq rdi ++
    cvtsi2sdq (reg rdi) (reg xmm0) ++
    pushf (reg xmm0)



(*fonction principale qui écrit le code pour l'arbre syntaxique expr dans le fichier appelé name*)
let compile expr name =   
  let typ = testype expr in 
  let dat = ref (printstr typ, 0) in
      let rec aux expr=
      match expr with
     | Number Int n -> store_int n
     | Number Float x -> 
      (*On rajoute ".FLi : .double x" dans la data pour stocker notre flottant, puis on met ce flottant dans la pile*)
       let (data, i) = !dat in dat := ((data ++ label (".FL" ^ (string_of_int i)) ++ inline ("  .double " ^ (string_of_float x) ^ "\n")), i + 1); 
       store_float x i
     | Unary (Minus, exp) -> if typ = "int" then aux exp ++ minus_int else aux exp ++ minus_float
     | Unary (Toint, exp) -> aux exp ++ toint 
     | Unary (Tofloat, exp) -> aux exp ++ tofloat
     | Binary (Add, exp1, exp2) -> aux exp1 ++ aux exp2 ++ binary_int addq
     | Binary (Sub, exp1, exp2) -> aux exp1 ++ aux exp2 ++ binary_int subq
     | Binary (Mul, exp1, exp2) -> aux exp1 ++ aux exp2 ++ binary_int imulq
     | Binary (Div, exp1, exp2) -> aux exp1 ++ aux exp2 ++ divide
     | Binary (Mod, exp1, exp2) -> aux exp1 ++ aux exp2 ++ modulo
     | Binary (Addf, exp1, exp2) -> aux exp1 ++ aux exp2 ++ binary_float addsd
     | Binary (Subf, exp1, exp2) -> aux exp1 ++ aux exp2 ++ binary_float subsd
     | Binary (Mulf, exp1, exp2) -> aux exp1 ++ aux exp2 ++ binary_float mulsd
    in
  let instructions = aux expr in
  let code = {text =
            main instructions typ;
          data =
            fst !dat ++
            label ".ZERO" ++ inline ("    .double 0.0\n");} (*on rajoute .ZERO pour faire la négation de flottants*)
  in
  write name code;;
