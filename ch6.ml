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

let rec insert_ordered x = function
Leaf -> Node(x,Leaf,Leaf)
| Node (y,left,right) as node -> 
  if x < y then
  Node(y,insert_ordered x left,right)
  else
  Node(y,left,insert_ordered x right);;


type comparison = LessThan | Equal | GreaterThan;;
let rec insert_ordered_comp x comparator tree = match tree with
Leaf -> Node(x,Leaf,Leaf)
| Node(y,left,right) as node -> match comparator x y with
LessThan -> Node(x,insert_ordered_comp x comparator left,right)
| _ -> Node(x,left,insert_ordered_comp x comparator right);;


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

(* let e_dict = empty_dict;;
let one_dict = add_dict 1 [2;3] e_dict;;
let two_dict = add_dict 2 [4] one_dict;;
let three_dict = add_dict 3 [4] two_dict;;

Printf.printf "%b\n" (reachable three_dict 1 4);; *)

(* repeating from above*)
type 'a tree = Node of 'a * 'a tree * 'a tree |
Leaf;;

type heap = int tree;;
let make_heap x = Node (x,Leaf,Leaf);;

let rec insert_heap x = function
Leaf -> Node(x,Leaf,Leaf)
| Node(y,l,r) as node ->  
     if x <= y then
     Node(x,Leaf,node)
     else
     Node(y,insert_heap x l,r);;

let find_min = function
Leaf -> (raise (Invalid_argument "Error"))
| Node(y,l,r) -> y;;

let heap_as_list h = 
let rec heap_as_lst_acc p acc = match p with
Leaf -> acc
| Node(x,l,r) -> let updated_acc = x::acc in 
                 let updated_acc_after_left = heap_as_lst_acc l updated_acc in
                 heap_as_lst_acc r updated_acc_after_left in
heap_as_lst_acc h [];;


let insert_heap_lst h lst = 
let rec insert_heap_lst_acc curr acc = match curr with
[] -> acc
| y::ys -> insert_heap_lst_acc ys (insert_heap y acc) in
insert_heap_lst_acc lst h;;

let meld h1 h2 = match h1 with
Leaf -> h2
| Node(x,xl,xr) as n1 -> match h2 with
    Leaf -> h1
    | Node(y,yl,yr) as n2 -> if x < y then
                        let lst =  heap_as_list n2 in
                        insert_heap_lst n1 lst
                        else
                        let lst =  heap_as_list n1 in
                        insert_heap_lst n2 lst;;

let delete_min = function
Leaf -> Leaf
| Node(_,l,r) -> meld l r;;
                 


let heap_sort h = 
let rec itr curr acc = match curr with
Leaf -> acc
| Node(x,l,r) as n1 -> let min_number = find_min n1 in
                       let updated_acc = min_number::acc in
                       let updated_heap = delete_min n1 in
                       itr updated_heap updated_acc in
itr h [];;

(* let rec heap_sort h = match h with
  Leaf -> []
| Node(x,_,_) -> (find_min h)::(heap_sort (delete_min h));; *)


let h10 = make_heap 10;;
let h2 = insert_heap 2 h10;;
let h1 = insert_heap 1 h2;;


let h11 = make_heap 11;;
let h3 = insert_heap 3 h11;;
let h0 = insert_heap 0 h3;;

let combined = meld h0 h1;;
let deleted_min = delete_min combined;;
let deleted_min_1 = delete_min deleted_min;;
let deleted_min_2 = delete_min deleted_min_1;;
let deleted_min_3 = delete_min deleted_min_2;;
let sorted_lst = List.rev (heap_sort combined);;



