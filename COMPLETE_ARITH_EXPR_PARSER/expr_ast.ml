
type no_val = NoVal;;
type common_expr = [
    `Expr of int * next  
]
and

next = [
    `PlusExpr of common_expr
  | `MinusExpr of common_expr
  | `TimesExpr of common_expr
  | `DivExpr of common_expr
  | `Noval
];;

let rec print_expr v = match v with
`Expr(i,next_expr) -> begin
  Printf.printf "Value is %d\n" i;
  print_next_expr next_expr
end
and
print_next_expr p = match p with
`PlusExpr(v) ->  print_expr v
| `MinusExpr(v) -> print_expr v
| `TimesEpxr(v) -> print_expr v
| `DivExpr(v) -> print_expr v
| `Noval -> Printf.printf "Finished";;


let rec eval_expr v = match v with
`Expr(i,next) -> eval_next i next
and
eval_next prev curr = match curr with
`PlusExpr (v) -> 
   let calculated_expr = eval_expr v in
   prev + calculated_expr
| `MinusExpr (v) ->
  let calculated_expr = eval_expr v in
   prev - calculated_expr
| `TimesExpr (v) ->
  let calculated_expr = eval_expr v in
   prev * calculated_expr
| `DivExpr (v) ->
  let calculated_expr = eval_expr v in
   prev / calculated_expr
| `Noval -> prev;;


