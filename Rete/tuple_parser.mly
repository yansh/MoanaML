%token <string> STRING
%token <string> VAR
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_SQUARE_BRACE
%token RIGHT_SQUARE_BRACE
%token START
%token END
%token COMMA

%token EOF


%start <Config.tuple list> parse_lst
%start <Config.tuple list list>parse_collection
%start <Config.tuple option> parse_tuple
%% 


parse_tuple:
  | t = tpl {Some (t)}
  | EOF    {None}
  ;
parse_lst:
  | t = dataset   {t}
  | EOF           {[]}
  ;
parse_collection:
  | t = collection {t};	
  | EOF           {[]}

dataset: START; t = stmts; END {t}
    
stmts:  stmt =  list(tpl);  {stmt}

collection: LEFT_SQUARE_BRACE; c= list (dataset); RIGHT_SQUARE_BRACE {c}

tpl:LEFT_BRACE; s = elem; COMMA; p = elem; COMMA; o = elem; COMMA; c  = elem; RIGHT_BRACE 
{{Config.subj = s; 
  Config.pred = p; 
    Config.obj  = o; 
    Config.ctxt = c; 
    Config.time_stp = None; 
    Config.sign = None}}  ;

elem:
 | v = VAR {Variable v}
 | c = STRING {Constant c} 
