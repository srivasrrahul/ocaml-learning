%token <string> ID
%token LEFT_BRACK
%token RIGHT_BRACK
%token COMMA
%token EOF
%token <string> STRING
%start <Nested_lst.element_lst> prog
%%

prog:
   l = list_parser {l}
;
list_parser:
  LEFT_BRACK;v= list_fields_parser;RIGHT_BRACK {v}
;
list_fields_parser:
  vl = separated_list(COMMA,list_field) {`PlainList vl}
;
list_field:
  i = ID {`Term i}
 | LEFT_BRACK;v= list_fields_parser;RIGHT_BRACK {`ListType v}

; 




  