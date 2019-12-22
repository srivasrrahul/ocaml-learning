let factorial i = 
  let j = ref 1 in
  for k = 2 to i do
    j := !j * k
  done;
  !j;;

(* Printf.printf "%d\n" (factorial 10) *)

type 'a elem = Nil | Elem of 'a * 'a elem ref * 'a elem ref;;

let nil_elem = Nil;;

let create_elem x = Elem (x,ref Nil,ref Nil);;

let get = function
Nil -> raise (Invalid_argument "Empty")
| Elem(x,_,_) -> x;;

let prev_elem = function
Nil -> raise (Invalid_argument "Empty")
| Elem(_,prev,_) -> !prev;;

type 'a dllist = 'a elem ref;;

let create() = Nil;;

let insert list elem = match 
elem,!list with 
Elem(_,prev,next),Nil -> 
  prev := Nil;
  next := Nil;
  list := elem;
| Elem(_,prev1,next1),(Elem(_,prev2,_) as head) ->
  prev1 := Nil;
  next1 := head;
  prev2 := elem;
  list := elem
| Nil,_ -> raise (Invalid_argument "insert");;

let remove list elem = match elem with
Elem (_, prev, next) -> (match !prev with
Elem (_, _, prev_next) -> prev_next := !next | Nil -> list := !next);
(match !next with
Elem (_, next_prev, _) -> next_prev := !prev
| Nil -> ()) | Nil ->
raise (Invalid_argument "remove");;


type 'a stackElement = Nil | StackElement of 'a * 'a stackElement ref;;



type 'a stack = 'a stackElement ref;;

let create_stack () = ref Nil;;

let push st element = 
match !st with
Nil -> let newStack = StackElement(element,ref Nil) in
       st := newStack
| StackElement(_,next) as current -> 
let newStack = StackElement(element,ref current) in
st := newStack;;

let pop st = match !st with
Nil -> raise (Invalid_argument "Empty Stack")
| StackElement(y,next) -> 
   st := !next;
   y;;


let st = create_stack ();;

push st 1;;
push st 2;;
push st 3;;


(* Printf.printf "%d\n" (pop st);;
Printf.printf "%d\n" (pop st);;
Printf.printf "%d\n" (pop st);; *)


type 'a binary_search_tree_node = Nil | Node of 'a * 'a binary_search_tree_node ref * 'a binary_search_tree_node ref;;

type 'a binary_search_tree = 'a binary_search_tree_node ref;;

let create_empty_search_tree = ref Nil;;

let rec add_node tree element = match !tree with
Nil -> let newTree = Node(element,ref Nil,ref Nil) in
       tree := newTree;
       tree
| Node(x,left,right) -> if element < x then
                         let updated_left = add_node left element in
                         left := !updated_left;
                         tree
                        else 
                          let updated_right = add_node right element in
                          right := !updated_right;
                          tree;;


let t = create_empty_search_tree;;
add_node t 10;;
add_node t 20;;
add_node t 5;;
add_node t 30;;
add_node t 7;;
add_node t 1;;


let rec visit f tree = match !tree with 
Nil -> raise (Invalid_argument "Bad logic")
| Node(x,l,r) -> match !l,!r with
                 Node(_,_,_) ,Node(_,_,_)  ->
                    visit f l;
                    f x;
                    visit f r;
                | Node(_,_,_) ,Nil ->
                   visit f l;
                   f x;
                | Nil,Node(_,_,_) ->
                   f x;
                   visit f r
                | Nil,Nil -> f x;;
                  

(* visit (Printf.printf "%d\n") t;; *)

let memo f =
 let table = ref [] in
 let rec find_or_apply entries x = 
 match entries with
 (x1,y1)::_ when x = x1 -> y1
 | _ :: pending_entries -> find_or_apply pending_entries x
 | [] ->
     let y = f x in
     table := (x,y)::!table;
     y
 in
 (fun x -> find_or_apply !table x);;

let rec fib = function
0 | 1 -> 1
| i -> fib (i-1) + fib (i-2);;

let fib_mem = memo fib;;

(* Printf.printf "%d\n" (fib_mem 40);; *)

type 'a lazyValue = Lazy of 'a ref | Computed of 'a;;
(* type 'a deferred = 'a Lazily ;; *)

type 'a deferred = Deferred of 'a lazyValue ref;;

let defer f =  Deferred(ref (Lazy f));;
              

let force deferred_val = match deferred_val with
Deferred(pointer) -> 
   match !pointer with
      Lazy(function_pointer) -> let y = !function_pointer in
                                pointer := Computed(y);
                                y
      | Computed(y) -> y;;

            

type 'a lazy_list = Nil
| Cons of 'a * 'a lazy_list
| LazyCons of 'a * 'a lazy_list deferred;;


let nil = Nil;;

let cons x = function
Nil -> Cons(x,Nil)
| Cons(y,ly) as curr -> Cons(x,curr)
| LazyCons(y,ly) as curr -> Cons(x,curr);;

let lazy_nil = (ref Nil);;


(* LazyCons(x,Deferred (ref (Lazy (ref Nil))));; *)



let lazy_cons x llist = function () ->
   let y = llist () in
   match y with
      Nil -> LazyCons(x,defer (ref Nil))
      | Cons(z,zs) as curr -> let pending_lazy_lst = defer (ref curr) in
                              LazyCons(x,pending_lazy_lst)
      | LazyCons(z,zs) as cur -> let pending_lazy_lst = defer (ref cur) in
                                 LazyCons(x,pending_lazy_lst);;


let is_nil llst = 
let y = llst in
match y with 
Nil -> true
| LazyCons(x,p) -> match p with
    Deferred(pointer) -> match !pointer with
      Lazy(nil_pointer) -> match !nil_pointer with
        Nil -> true
        | _ -> false
      | _ -> false
    | _ -> false
| _ -> false;;


let head llst = match llst with
Nil -> Nil
| Cons(x,_) -> x
| LazyCons(x,_) -> x;;

let tail llst = match llst with
Nil -> Nil
| Cons(_,xs) -> xs
| LazyCons(_,ys) -> force ys;;


let merge l1 l2 = let merge_local in
match l1,l2 with
LazyCons(x,xs) as a,_ -> (defer (ref a))
| Nil,LazyCons(y,ys) as b -> (defer (ref b))
merge_local;;

(* let rec merge l1 l2 = fun () ->
  match l1,l2 with
  Nil,Nil -> Nil
  | LazyCons(_,xs),LazyCons(y,ys) as ylist ->
    let pending_lazy_list1 = defer (ref xs) in
    let pending_lazy_list2 = defer (ref ylist) in
    let merged_lazy_lst = merge (force xs) (force ylist) in
    merged_lazy_lst;;


let (@@) l1 l2 = match l1 l2 with
Nil,Nil -> Nil
| Cons(x,xs),Cons(y,ys) as ylist -> 
   let pending_lazy_list1 = defer (ref xs) in
   let pending_lazy_list2 = defer (ref ylist) in
   let merged_lazy_lst = merge pending_lazy_list1 pending_lazy_list2 in
   merged_lazy_lst;;
        *)
                    
        

        
      
             




