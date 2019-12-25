
open Batteries;;
(* This is an integral min heap *)
(*ocamlfind ocamlc -package batteries -linkpkg heap.ml -o heap*)

type heap = Heap of int * int ref * int array ;;

let make_empty_heap n = Heap (n,ref 0,Array.make n 1);;

let parent i = i/2;; 

let print_heap = function
Heap(_,pointer_curr_size,arr) ->
for j = 0 to (!pointer_curr_size-1) do
 Printf.printf "%d," arr.(j);
 done;
Printf.printf "\n";;

let left index = 2*(index+1)-1;;
let right index = 2*(index+1);;
let exists index max_size = index < max_size;;


let add_element h element = match h with
Heap(max_size,current_size,arr) -> 
  if max_size = !current_size then
     raise (Invalid_argument "Not enough space")
  else
    begin
       arr.(!current_size) <- element;
       current_size := !current_size + 1;
       (* heapify h (!current_size-1); *)
    end;;

let smallest_element = function 
Heap(_,curr_size,arr) -> 
   if (!curr_size = 0) then
     raise (Invalid_argument "No element")
   else
     arr.(0);;



let swap arr i j = 
let temp = arr.(i) in
arr.(i) <- arr.(j);
arr.(j) <- temp;;

let rec heapify h index =
match h with
Heap(max_size,curr_size,arr) -> 
begin
   if (index < !curr_size) then
     begin
        let ref_min_index = ref index in
        let left_index = left index in
        let right_index = right index in
        if (exists left_index !curr_size && arr.(!ref_min_index) > arr.(left_index)) then
            begin
            ref_min_index := left_index;
            end;
        if (exists right_index !curr_size && arr.(!ref_min_index) > arr.(right_index)) then
            begin
            ref_min_index := right_index;
            end;
        
        if (!ref_min_index != index) then
           begin
             swap arr index !ref_min_index;
             heapify h !ref_min_index;
           end
     end
   else
      Printf.printf "loop ends\n"

end;;

  
let remove_smallest = 
function 
Heap(_,curr_size,arr) as h -> 
   (* Printf.printf "size : %d\n" !curr_size; *)
   if (!curr_size = 0) then
     raise (Invalid_argument "No element")
   else
     begin
       arr.(0) <- arr.(!curr_size-1);
       curr_size := !curr_size-1;
       heapify h 0;
     end;;

let build_min_heap = function 
Heap(_,pointer_curr_size,arr) as h ->
for j = ((!pointer_curr_size)/2) downto 0 do
  (* Printf.printf "j = %d \n" j; *)
  (* print_heap h; *)
  heapify h j;
  (* print_heap h; *)
  (* Printhf.printf "=======\n"; *)
done;;


let heap_sort arr = 
  let l = Array.length arr in
  let h = Heap (l,ref l,arr) in
  build_min_heap h;
  match h with
  Heap(_,pointer_to_heap_len,_) as h1-> 
  for j = (l-1) downto 0 do
    swap arr j 0;
    pointer_to_heap_len := !pointer_to_heap_len-1;
    heapify h1 0;
  done;

  Array.rev_in_place arr;;


let decrease_key h index key = 
match h with 
Heap(l,pointer_to_size,arr) ->
if (index >= !pointer_to_size || arr.(index) < key) then
  raise (Invalid_argument "key is smaller than original")
else
  begin
    arr.(index) <- key;
    let current_index = ref index in
    let parent_index = ref (parent index) in
    while !parent_index != !current_index && arr.(!parent_index) > arr.(!current_index) do
       swap arr !parent_index !current_index;
       current_index := !parent_index;
       parent_index := (parent !current_index);
    done;
          
  end;;

let insert h key = 
match h with
Heap(max_size,pointer_curr_size,arr) ->
if (!pointer_curr_size >= max_size) then
 raise (Invalid_argument "Max reached")
else
 begin
    arr.(!pointer_curr_size) <- max_int;
    pointer_curr_size := !pointer_curr_size + 1;
    decrease_key h (!pointer_curr_size-1) key;
 end;;



 



(* let h = make_empty_heap 10;;
add_element h 4;;
add_element h 3;;
add_element h 2;;
add_element h 1;;
add_element h 0;;
add_element h 5;;
add_element h 6;;

build_min_heap h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;; *)

(* let arr = [|23;2;33;11;12;17;8|] in
heap_sort arr;
Array.iter (Printf.printf "%d, ") arr;;
Printf.printf "\n";; *)

let h = make_empty_heap 10;;

insert h 5;;
insert h 4;;
insert h 3;;
insert h 6;;
insert h 0;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;

Printf.printf " smallest : %d\n" (smallest_element h);;
remove_smallest h;;











      


