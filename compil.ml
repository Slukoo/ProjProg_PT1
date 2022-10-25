open Format
open X86_64


type exp = Int of int | Add of exp*exp ;;

let asm_printint =
inline "
print_int:
    movq %rdi, %rsi
    movq $S_int, %rdi
    movq $0, %rax
    call printf
    ret
";;

let sint =
  label "S_int" ++ string "%d";;


let asm_int n regi =
           movq (imm n) (reg regi)
       ;;


let asm_sum c1 c2 regi=
  c1 ++ c2 ++
  addq (reg rbx) (reg rbp) ++
  movq (reg rbp) (reg regi)

  ;;


let write name code =
  let file = open_out name in
  let fmt = formatter_of_out_channel file in
  X86_64.print_program fmt code;
  close_out file;;





let compile expr name =
  let rec aux expr regi =
    match expr with
    | Int n -> asm_int n regi
    | Add (e,e') -> asm_sum (aux e rbx) (aux e' rbp) regi
  in
  let code = {text =
            globl "main" ++ label "main" ++
            (aux expr rdi) ++
            call "print_int" ++
            ret ++
            asm_printint;
          data =
            sint;} in
  write name code;;


compile (Int 23) "test_int.s";;
compile (Add (Int 1, Int 2)) "test_sum.s";;
compile (Add (Int 1, Add (Int 2, Int 3))) "test_sum2.s";;
