let swap arr i j = 
let temp = arr.(i) in
arr.(i) <- arr.(j);
arr.(j) <- temp;;

let partition arr p r = 
    (* Printf.printf "%d,%d\n" p r; *)
    let x = arr.(r) in
    let i = ref (p-1) in
    for j = p to (r-1) do
        if (arr.(j) < x) then
            begin
                i := !i + 1;
                swap arr !i j;
            end 
    done;
    swap arr (!i+1) r;
    !i+1;;

let quick_sort arr =
    let rec sort a b = 
        if b > a then 
            begin
                let pivot = partition arr a b in
                (* Printf.printf "Pivot is %d\n" pivot; *)
                sort a (pivot-1);
                sort (pivot+1) b ; 
            end
            in
    sort 0 ((Array.length arr)-1);;


let arr = [|10;12;2;3;4;7;13|] in
quick_sort arr;
Array.iter (Printf.printf "%d,") arr;;