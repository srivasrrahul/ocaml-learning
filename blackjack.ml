open Scanf;;
let a,b,c = scanf "%d %d %d\n" (fun i j k -> i,j,k);;
let r = a + b + c;;
if r >= 22 then
 Printf.printf "bust\n"
else
 Printf.printf "win\n"