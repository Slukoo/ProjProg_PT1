open Asyntax
open Lexer
open Parser
open Compil


let _ =
  let lexbuf = Lexing.from_channel (open_in (Sys.argv.(1))) in  (*on ouvre le fichier donner en argument dans le terminal*)
  let ast = Parser.parse Lexer.token lexbuf in  (*on crée l'arbre syntaxique de l'expression donnée*)
  compile ast "out.s" (*on transforme cet arbre syntaxique en code assembleur*)

 
