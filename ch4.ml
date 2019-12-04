let zero = 0;;
let one = 1;;

let rec fib  = function
 k when k < 2 -> k
| k -> fib (k-1) + fib(k-2);;

let transform = function 
   k when k = 'A' -> 'C'
 | k when k = 'B' -> 'A'
 | k when k = 'C' -> 'D'
 | k when k = 'D' -> 'B'
 | _ -> raise (Invalid_argument "outside set");; 

let string_transform s = String.map transform s;;

(* Printf.printf "%d" (fib 25) *)
Printf.printf "%s\n" (string_transform "ABC")