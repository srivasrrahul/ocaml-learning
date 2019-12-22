open Scanf;;
let s = scanf "%s\n" (fun x -> x);;
let len = String.length s;;
let palin_len = len/2;;
let diff = ref 0;;
for k=0 to palin_len-1 do
  let remote_index = len-1-k in
  if s.[k] != s.[remote_index] then begin
  (* Printf.printf "%d\n" k; *)
  diff := !diff + 1
  end;
done;;
Printf.printf "%d\n" !diff;;