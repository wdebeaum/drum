sexp
  = sp val:(list / atom) sp { return val }

list
  = '(' elts:(sexp*) sp ')' { return elts }

atom
  = string
  / number
  / symbol

string
  = $('"' ("\\" . / [^\\\"])* '"')

number
  = str:$(
      [+-]? (
	[0-9]* '.' [0-9]+
      / [0-9]+ '.'?
      )
    ) { return parseFloat(str) }

symbol
  = $((symbol_component '::' / ':')? symbol_component)

symbol_component
  = [^ \n\r\t()|:]+
  / '|' ("\\" . / [^\\|])* '|'

sp
  = ( [ \n\r\t] 
    / ';' [^\r\n]* [\r\n]
    )*

