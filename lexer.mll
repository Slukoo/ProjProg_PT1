{
 open Parser
 exception Eof
}


rule token = parse
  [' ' '\t'] { token lexbuf }
| '\n'	   { EOL }
| '('      { LBRACE }
| ')'      { RBRACE }
| "+."	   { ADDF }
| "-."     { SUBF }
| "*."     { MULF }
| '+'      { ADD }
| '-'	   { SUB }
| '*'      { MUL }
| '/'      { DIV }
| '%'      { MOD }
| 'i'	   { TOINT }
| 'f'	   { TOFLOAT }
| ['0'-'9']+ '.'['0'-'9']+ as flo { FLOAT (float_of_string flo) } 
| ['0'-'9']+ as ent { INT (int_of_string ent) } 
| eof { raise Eof } 