let binary_search arr needle =
 let first = ref 0 in
 let mid = ref ((Array.length arr)/2) in
 let end_arr = ref ((Array.length arr)-1) in
 let result = ref (-1) in
 let termintate = ref false in
 while !first <= !end_arr && !termintate == false do
   (* Printf.printf "%d %d\n" !first !end_arr; *)
   mid := !first + (!end_arr - !first)/2;
   if arr.(!mid) > needle then
     begin
       end_arr := !mid-1
     end
   else
      if arr.(!mid) < needle then
        begin
          first := !mid+1;
        end
      else
         begin
          result := !mid;
          termintate := true;
         end;
    
    if !first > !end_arr then
      termintate := true;

 done;
 !result;;


 let arr = [|1;3;5;7;9;11;13;15;17;19|] in
 Printf.printf "%d\n" (binary_search arr (3));;
