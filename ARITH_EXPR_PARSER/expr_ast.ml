
type common_expr = [
  `Expr of int * plus_expr  
]
and
plus_expr = [
  `PlusExpr of common_expr
  | `Noval
];;


let rec print_expr v = match v with
`Expr(i,plus_expr) -> begin
  Printf.printf "Value is %d\n" i;
  print_plus_expr plus_expr
end
and
print_plus_expr p = match p with
`PlusExpr(v) ->  print_expr v
| `Noval -> Printf.printf "Finished";;


let rec eval_expr v = match v with
`Expr(i,next) -> eval_next i next
and
eval_next prev curr = match curr with
`PlusExpr (v) -> 
   let calculated_expr = eval_expr v in
   prev + calculated_expr
| `Noval -> prev;;


