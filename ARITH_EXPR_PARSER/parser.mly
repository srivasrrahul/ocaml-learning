%token <int> INT
%token PLUS
%token EOF
%token <string> STRING
%start <Expr_ast.common_expr option> prog
%%

prog:
   v = value {Some(v)}
;
value :
  | i = INT; v = next { `Expr (i,v) }

  
;
next :
 | EOF {`Noval}
 | PLUS;v=value {`PlusExpr v};

  