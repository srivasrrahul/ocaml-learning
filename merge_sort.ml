let merge arr start mid end_arr = 
  (* Printf.printf "Merging index : start = %d mid= %d end= %d\n" (start) mid end_arr; *)
  let len = (end_arr-start+1) in
  let temp = Array.make len arr.(start) in
  let first_index = ref start in
  let second_index = ref (mid+1) in
  let sorted_index = ref 0 in
  while !first_index <= mid && !second_index <= end_arr do
     if (arr.(!first_index) < arr.(!second_index)) then begin
        temp.(!sorted_index) <- arr.(!first_index);
        sorted_index := !sorted_index + 1;
        first_index := !first_index + 1;
        end
     else
        begin
           temp.(!sorted_index) <- arr.(!second_index);
           sorted_index := !sorted_index + 1;
           second_index := !second_index + 1;
        end;
  done;

  if !first_index <= mid then begin
      while !first_index <= mid do
        temp.(!sorted_index) <- arr.(!first_index);
        sorted_index := !sorted_index + 1;
        first_index := !first_index + 1;
      done;
  end;

  if !second_index <= end_arr then begin
      while !second_index <= end_arr do
        temp.(!sorted_index) <- arr.(!second_index);
        sorted_index := !sorted_index + 1;
        second_index := !second_index + 1;
      done;
  end;
  
  (* Printf.printf "\n";
  Printf.printf "Merging index result: start = %d mid = %d end = %d\n" (start) mid end_arr;
  Array.map (Printf.printf "[%d],") temp;
  Printf.printf "\n"; *)
  Array.blit temp 0 arr start len;;


let merge_sort arr =
let rec merge_sort_internal arr start_index end_index = 
   (* Printf.printf "%d,%d\n" (start_index) end_index; *)
   if start_index >= end_index then
      ()
   else 
      begin
      (* Printf.printf "Second place : %d,%d\n" (start_index) end_index; *)
      let mid = start_index + (end_index-start_index)/2 in
      merge_sort_internal arr start_index mid ;
      merge_sort_internal arr (mid+1) end_index ;
      merge arr start_index mid end_index
      end in
   merge_sort_internal arr 0 ((Array.length arr) - 1);;


let arr = [|10;2;9;11;12;7;-1|] in
merge_sort arr;
Array.map (Printf.printf "%d,") arr;;

