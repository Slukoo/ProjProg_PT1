open Asyntax
open Lexer
open Parser
open Compil


let _ =
  let lexbuf = Lexing.from_channel (open_in (Sys.argv.(1))) in  (* Mettre stdin pour lire directement le texte ecrit dans la console *)
  let ast = Parser.parse Lexer.token lexbuf in
  compile ast "out.s"

 
