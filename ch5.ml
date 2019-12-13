let identity x = x;;

let square x = x * x;;

(* Printf.printf "%d\n" ((identity square) 2);; *)

let rec sum = function
   [] -> 0
 | x::xs -> x + (sum xs);;
 

 (* Printf.printf "%d\n" (sum [1;2;3]);; *)

 let rec mem y l = 
   match l with
   [] -> false
   | x::xs -> (x=y) || (mem y xs);;

(* Printf.printf "%b\n" (mem 4 [1;2;3]);; *)

let rec map f = function
 [] -> []
 | x::xs -> f x :: map f xs;;


let db =
["John", "x3456", 50.1;
"Jane", "x1234", 107.3; "Joan", "unlisted", 12.7];;

(* let rec find_salary x d = 
match d with
(a,t,u)::ds when (a=x) -> (a,t,u)
| _::ds -> (find_salary x ds)
| _ -> raise (Invalid_argument "outside db");; *)

let find_salary x = 
let rec find_sal_rec d = 
match d with
(a,t,u)::ds when (a=x) -> (a,t,u)
| _::ds -> (find_sal_rec ds)
| _ -> raise (Invalid_argument "outside db") in
find_sal_rec db;;

(* let (x1,x2,x3) = find_salary "Jane" ;;
Printf.printf "%s %s %f\n" x1 x2 x3 *)


let select p =
let rec select_rec d acc =
match d with 
d1::ds when (p d1) = true -> (select_rec ds (d1::acc))
| _::ds -> (select_rec ds acc)
| [] -> acc in
select_rec db [];;

let x = select (fun (_,_,salary) -> salary < 100.0) ;;

let rec print_iter e = match e with 
(a,b,c)::xs -> (Printf.printf "%s %s %f\n" a b c);
               (print_iter xs);
| [] -> [];; 

(* print_iter x;; *)


let append l1 l2 =
let rec append_tail x y acc =
match x with
x1::xs -> (append_tail xs y (x1::acc))
| [] -> 
  match y with
  y1::ys -> (append_tail [] ys (y1::acc))
  | [] -> List.rev acc
in
append_tail l1 l2 [];;


let c = append [1;2;3] [4;5;6];;

(* List.iter (Printf.printf "%d,") c;; *)

let rec welfare_crook x y z = 
 match x with
 x1::xs when (mem x1 y) && (mem x1 z) -> x1
 | x1::xs -> welfare_crook xs y z
 | [] -> raise (Invalid_argument "No such element");;

 Printf.printf "%d" (welfare_crook [1;2;3] [2;10;12] [-10;-5;2]);;