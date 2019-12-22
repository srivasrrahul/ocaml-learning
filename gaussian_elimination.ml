let reduce_step x y row_size col_size row_to_be_reduced col_to_be_reduced base_row= 
     let factor = x.(row_to_be_reduced).(col_to_be_reduced) /. x.(base_row).(col_to_be_reduced) in
     for j = col_to_be_reduced to col_size do
       x.(row_to_be_reduced).(j) <- x.(row_to_be_reduced).(j) -. factor *. x.(base_row).(j);
     done;
     y.(row_to_be_reduced).(0) <- y.(row_to_be_reduced).(0) -. factor *. y.(base_row).(0);;




let gaussian_elimination x y row_size col_size = 
  let max_row = ref 1 in
  for j = row_size downto !max_row do
    reduce_step x y row_size col_size j 0 0
  done;
  max_row := !max_row + 1;
  Printf.printf "Max row is%d\n" (!max_row);
  for j = row_size downto !max_row do
    reduce_step x y row_size col_size j 1 (j-1)
  done;;

let x = Array.make_matrix 3 3 1.0;;
let y = Array.make_matrix 3 1 1.0;;

x.(0).(1) <- -3.0;;
x.(0).(2) <- 5.0;;

x.(1).(0) <- 2.0;;
x.(1).(1) <- -1.0;;
x.(1).(2) <- -3.0;;

x.(2).(0) <- 3.0;;
x.(2).(1) <- 1.0;;
x.(2).(2) <- 4.0;;


y.(0).(0) <- -9.0;;
y.(1).(0) <- 19.0;;
y.(2).(0) <- -13.0;;

gaussian_elimination x y 2 2;

x;;
y;;
