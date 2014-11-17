%token <string> STRING
%token <string> VAR
%token COMMA

%token MAP
%token START
%token END

%token EOF

%start <Config.tuple list> parse
%% 




parse:
 | q = query {q}
 | EOF {[]} 

query:  MAP; START; q = stmts; END {q}

stmts: stmt =  list(tpl); {stmt}

tpl:  s = elem; COMMA; p = elem; COMMA; o = elem; COMMA; c  = elem; 
{{Config.subj = s; Config.pred = p; Config.obj = o; Config.ctxt = c; Config.time_stp = None; Config.sign = None}}  ;

elem:
 | v = VAR {Variable v}
 | c = STRING {Constant c} 


