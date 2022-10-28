{
 open Parser
}


rule token = parse
  [' ' '\t'] { token lexbuf }
| '\n'	   { EOL }
| '('      { LBRACE }
| ')'      { RBRACE }
| "**"     { POW }
| "+."	   { ADDF }
| "-."     { SUBF }
| "*."     { MULF }
| '+'      { ADD }
| '-'	     { SUB }
| '*'      { MUL }
| '/'      { DIV }
| '%'      { MOD }
| '!'      { FACT }
| "int"	   { TOINT }
| "float"	 { TOFLOAT }
| ['0'-'9']+ '.'['0'-'9']+ as flo { FLOAT (float_of_string flo) } 
| ['0'-'9']+ as ent { INT (int_of_string ent) } 
| eof { EOL } (*on traite la fin du fichier comme une fin de ligne*)