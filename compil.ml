open Format
open X86_64

type nombre = Int of int | Float of float
type binaire = Add | Sub | Mul | Div | Mod | Addf | Subf | Mulf 
type unaire = Toint | Tofloat | Minus

type sexp = 
  | Number of nombre
  | Unary of unaire * sexp
	| Binary of binaire * sexp * sexp


let write name code =
  let file = open_out name in
  let fmt = formatter_of_out_channel file in
  X86_64.print_program fmt code;
  close_out file;;


let binary_int op =
	popq rdi ++
	popq rsi ++
	op (reg rdi) (reg rsi) ++
	pushq (reg rsi);;

let divide =
	movq (imm 0) (reg rdx) ++
	popq rax ++
	popq rdi ++
	idivq (reg rdi) ++
	pushq (reg rax);;

let modulo =
	movq (imm 0) (reg rdx) ++
	popq rax ++
	popq rdi ++
	idivq (reg rdi) ++
	pushq (reg rdx);;


let store_int n =
	pushq (imm n);;




let compile expr name =
  let rec aux expr =
    match expr with
    | Number Int n -> store_int n
    | Binary (Add,exp1,exp2) -> aux exp1 ++ aux exp2 ++ binary_int addq
    | Binary (Sub,exp1,exp2) -> aux exp1 ++ aux exp2 ++ binary_int subq
    | Binary (Mul,exp1,exp2) -> aux exp1 ++ aux exp2 ++ binary_int imulq
    | Binary (Div,exp1,exp2) -> aux exp1 ++ aux exp2 ++ divide
    | Binary (Mod,exp1,exp2) -> aux exp1 ++ aux exp2 ++ modulo
    | _ -> failwith "Cas non traité"
  in
  let code = {text =
            globl "main" ++ label "main" ++
            (aux expr) ++
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
";
          data =
            label "S_int" ++ string "%d";} in
  write name code;;


compile (Number (Int 23)) "test_int.s";;
compile (Binary (Add, Number (Int 1), Number (Int 2))) "test_sum.s";;
