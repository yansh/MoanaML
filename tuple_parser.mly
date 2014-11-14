%token <string> STRING
%token LEFT_BRACE
%token RIGHT_BRACE
%token COMMA
%token EOF

%start <Config.tuple option> tuple
%%


tuple:
  | t = tpl { Some (t) }
  | EOF       { None }
  ;
	
tpl:  LEFT_BRACE; s = STRING; COMMA; p = STRING; COMMA; o = STRING; COMMA; c  = STRING; RIGHT_BRACE 
{{subj = Constant s;
pred = Constant p;
obj = Constant o;
ctxt = Constant c;
time_stp = None;
sign = None}}  ;

