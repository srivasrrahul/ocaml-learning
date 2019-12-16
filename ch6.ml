type number = Zero |
Integer of int |
Float of float;;

let z = Zero;;
let i = Integer 1;;

let x = Float 0.24;;

type 'a tree = Node of 'a * 'a tree * 'a tree |
Leaf;;

let n = Leaf;;
let n1 = Node (1,n,n);;

let rec cardinality = function
Leaf -> 0
| Node (_,l,r) -> 1 + (cardinality l) + (cardinality r);;

Printf.printf "%d" (cardinality n1);;

let empty = Leaf;;
let insert x s = Node (x,Leaf,s);;

let rec set_of_lst = function
[] -> empty
| x::xs -> insert x (set_of_lst xs);;

let rec mem x = function
Leaf -> false
| Node (y,l,r) -> (x=y) || (mem x l) || (mem x r);;

let string_of_number1 n =
match n with
`Integer i -> string_of_int i
| _ -> raise (Invalid_argument "Unkown Type");;

let l = [1;2;3;4;5];;

type 'a mylist = Nil | Cons of 'a * 'a mylist;;

let rec map f = function
Nil -> Nil
| Cons(x,xs) -> Cons((f x),(map f xs));;

let rec append x y = match x with
Nil -> y
| Cons(x,xs) -> Cons(x,(append xs y));;


type unary_number = Z | S of unary_number;;

let rec add_unary u1 u2 = 
match u1 with
Z -> u2
| S(u) -> S(add_unary u u2);;

let rec multiply_unary u v = 
match u with
Z -> Z
| S(u1) -> match v with
Z -> Z
| _ -> let rec add_times t res = 
match t with
S(Z) -> res
| S(x) -> add_times (x) (add_unary v res) in
add_times u v;;


type small = Four | Three | Two | One;;
let lt_small x y = match x with
Four -> false
| Three -> match y with
Four -> true
| _ -> false;
| Two -> match y with
Three -> true
| Four -> true
| _ -> false;
|One -> match y with
One -> false
| _ -> true;;

type unop = Neg;;
type binop = Add | Sub | Mul | Div;;
type exp = Constant of int
| Unary of unop * exp
| Binary of exp * binop * exp;;

let rec eval ex = match ex with
Constant(i) -> i
| Unary(u,e1) -> -1*(eval e1)
| Binary(e1,Add,e2) -> (eval e1) + (eval e2)
| Binary(e1,Sub,e2) -> (eval e1) - (eval e2)
| Binary(e1,Mul,e2) -> (eval e1) * (eval e2)
| Binary(e1,Div,e2) -> (eval e1) / (eval e2);;

let c1 = Constant(2);;
let c2 = Constant(3);;
let e = Binary(c1,Add,c2);;

Printf.printf "\n%d\n" (eval e);;

type ('key,'value) dictionary = NilValue 
| Node of 'key * 'value * ('key,'value) dictionary * ('key,'value) dictionary;;

type 'v result = NoValue | Some of 'v;;

let empty_dict = NilValue;;

let add_dict k v dict = Node (k,v,NilValue,dict);;

let rec find_dict k dict = match dict with
NilValue -> NoValue
| Node(key,v,l,r) when key=k -> Some(v)
| Node(key,v,l,r) -> 
  let lval = find_dict k l in 
  match lval with
  NoValue -> find_dict k r
  | _ -> lval;;




type vertex = int;;
type graph = (vertex, vertex list) dictionary;;

let find_neigbours g v = 
match find_dict v g with
Some(x) -> x
| NoValue -> [];;

let rec reachable graph s v = 
(*match s with
 v when s==v -> true
| _ ->  *)
if s!=v then
let rec visit visited_already next_tobe_visited =
match next_tobe_visited with
x::xs when x=v -> true
| x::xs when (List.mem x visited_already) = false -> 
  visit (List.append visited_already [x]) (List.append next_tobe_visited (find_neigbours graph x))
| x::xs -> visit visited_already xs
| [] -> false in
visit [s] (find_neigbours graph s)
else
  true;;

(*1,2,3,4 1->[2,3], 2->[4],3->[4]*)

let e_dict = empty_dict;;
let one_dict = add_dict 1 [2;3] e_dict;;
let two_dict = add_dict 2 [4] one_dict;;
let three_dict = add_dict 3 [4] two_dict;;

Printf.printf "%b\n" (reachable three_dict 1 4);;
