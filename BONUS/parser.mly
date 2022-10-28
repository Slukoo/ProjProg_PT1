 /* File parser.mly */
        %token <int> INT 
		%token <float> FLOAT
        %token ADDF SUBF MULF ADD SUB MUL DIV MOD POW FACT
        %token LBRACE RBRACE
		%token TOINT TOFLOAT
        %token EOL
        %left ADD SUB ADDF SUBF POW    
        %left MUL DIV MOD MULF         
        %nonassoc MINUS PLUS TOINT TOFLOAT FACT
		%start parse

		%type <Asyntax.sexp> parse
		%%

parse:
	| exp EOL		{ $1 }
;	

exp:
	| INT							{ Asyntax.Number (Int $1) }
	| FLOAT							{ Asyntax.Number (Float $1) }
	| LBRACE exp RBRACE 			{ $2 }
	| exp POW exp					{ Asyntax.Binary (Pow, $1, $3)}
	| exp ADD exp 					{ Asyntax.Binary (Add, $1, $3)}
	| exp SUB exp					{ Asyntax.Binary (Sub, $1, $3)}
	| exp MUL exp					{ Asyntax.Binary (Mul, $1, $3)}
	| exp DIV exp					{ Asyntax.Binary (Div, $1, $3)}
	| exp MOD exp					{ Asyntax.Binary (Mod, $1, $3)}
	| exp ADDF exp      			{ Asyntax.Binary (Addf, $1, $3)}
	| exp SUBF exp					{ Asyntax.Binary (Subf, $1, $3)}
	| exp MULF exp					{ Asyntax.Binary (Mulf, $1, $3)}
	| TOINT LBRACE exp RBRACE		{ Asyntax.Unary (Toint, $3)}
	| TOFLOAT LBRACE exp RBRACE 	{ Asyntax.Unary (Tofloat, $3)}
	| exp FACT						{ Asyntax.Unary (Fact, $1)}
	| ADD exp %prec PLUS			{ $2 }
	| SUB exp %prec MINUS   		{ Asyntax.Unary (Minus, $2)}

