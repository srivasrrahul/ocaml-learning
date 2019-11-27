let deriv f dx = 
    (fun x -> ((f (x +. dx) -. (f x))/. dx));;

let square1 x = x*.x;;

let deriv_square = deriv square1 0.0001;;

let der = deriv_square 3.0;;

(* Printf.printf "%f" der;; *)

let f ~x ~y = x - y;;

(* Printf.printf "%d" (f 10 2);; *)


let rec sum a b f = 
   if a ==b then
      f(b)
   else
      f(a) + (sum (a+1) b f)

let square x = x * x;;
let r_sum = sum 1 3 square;;

(* Printf.printf "%d\n" r_sum;; *)

let rec gcd a b = 
   if a < b then
      gcd b a
    else
      if b == 0 then
        a
      else
          let d = a / b in
          let r = a mod b in
          if (r == 0) then
            b
          else
            gcd d r;;

(* Printf.printf "%d" (gcd 1026 64);; *)

let valid_point f x = (f x) >=0 && (f (x-1)) < 0;;

let search f n = 
    let rec search_internal a b =
    if (a >= b) then 
      a
    else 
    let mid = (b + a)/2 in
    if (valid_point f mid) then
        mid
    else
        if (f mid) > 0 then
          search_internal a (mid-1)
        else
          search_internal (mid+1) b
    in
    search_internal 0 n

(* let test_f x =
  if x < 0 then
    -100
  else
     x-99;;


let test_val = search test_f 1000;;

Printf.printf "%d" test_val *)


let empty = 
   fun k -> 0;;

let add dict key v = 
   fun k -> 
     if (k = key) then
       v
     else
       (dict k);;

let search_dict dict key = dict key;;


(* let d1 = empty;; *)
let d2 = add empty "x" 1;;
let d3 = add d2 "y" 2;;
let d4 = add d3 "z" 3;;
let d5 = add d4 "w" 4;;

Printf.printf "%d\n" (search_dict d5 "y");;
         
   
