open Format
open X86_64
open Asyntax

let write name code =
  let file = open_out name in
  let fmt = formatter_of_out_channel file in
  X86_64.print_program fmt code;
  close_out file;;

let main instructions typ =
  if typ = "int" then
  globl "main" ++ label "main" ++
            instructions ++
	          popq rdi ++
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

  else
    globl "main" ++ label "main" ++
              instructions ++
              inline "    movsd (%rsp), %xmm0\n" ++
              addq (imm 8) (reg rsp) ++
              call "print_float" ++
              ret ++
              inline "
  print_float:
	    movq $S_float, %rdi
	    movq $1, %rax
	    call printf
	    ret
";;


let printstr typ =
  if typ = "float" then label "S_float" ++ string "%f"
  else label "S_int" ++ string "%d";;

let binary_int op =
	popq rdi ++
	popq rsi ++
	op (reg rdi) (reg rsi) ++
	pushq (reg rsi)

let binary_float op =
  popf (reg xmm0) (reg rsp)++
  popf (reg xmm1) (reg rsp)++
  op (reg xmm1) (reg xmm0) ++
  pushf (reg xmm0) (reg rsp)

let divide =
	movq (imm 0) (reg rdx) ++
	popq rdi ++
	popq rax ++
	idivq (reg rdi) ++
	pushq (reg rax)

let modulo =
	movq (imm 0) (reg rdx) ++
	popq rdi ++
	popq rax ++
	idivq (reg rdi) ++
	pushq (reg rdx)


let minus_int =
  popq rdi ++
  movq (imm 0) (reg rsi) ++
  subq (reg rdi) (reg rsi) ++
  pushq (reg rsi)

let minus_float =
    popf (reg xmm0) (reg rsp)++
    movfl ".ZERO(%rip)" (reg xmm1) ++
    subsd (reg xmm0) (reg xmm1) ++
    pushf (reg xmm1) (reg rsp)


let toint =
    popf (reg xmm0) (reg rsp)++
    cvttsd2si (reg xmm0) (reg rdi) ++
    pushq (reg rdi)

let tofloat =
    popq rdi ++
    cvtsi2sdq (reg rdi) (reg xmm0) ++
    pushf (reg xmm0) (reg rsp)

let store_int n =
	pushq (imm n);;


let store_float x i = 
  movfl (".FL" ^ (string_of_int i) ^ "(%rip)") (reg xmm0) ++
  pushf (reg xmm0) (reg rsp)

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




let compile expr name =
  let typ = testype expr in
  let dat = ref (printstr typ, 0) in
      let rec aux expr=
      match expr with
     | Number Int n -> store_int n
     | Number Float x -> 
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
            label ".ZERO" ++ inline ("    .double 0.0\n");} in
  write name code;;
