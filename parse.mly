%{

open Printf
open List
type sheet = float list list;;
type index = float * float ;;
type range = index * index ;;
(* comments *)


let m = 6;; 
let n = 6;;
(* for conversion of string list to float list*)

let rec print_list stry : float list =  
match stry with
[] ->  []
| e::l -> 

if e = "" then 
nan :: (print_list l) 
else
float_of_string e :: (print_list l)

(*for printing string list*)

let rec print_list_f = function 
[] -> ()
| e::l -> Printf.printf "%f" e ; print_string " "; print_list_f l

(*for printing list after each operation *)
let string_of_float_list l = String.concat " " (List.map string_of_float l);;
let float_list_to_string l = String.concat "\n" (List.map string_of_float_list l);;

(*for converting a csv file to a float float list*)
let pick =
  try
  let  l = ref [] in
    let in_stream = open_in Sys.argv.(1) in
      for i=0 to (m-1) do
          let line = input_line in_stream in
          let split = Str.split_delim(Str.regexp ",") in
          let values = split line in
          let s =  (print_list values : float list) in
          l := !l @ [s]
        done;
        close_in in_stream; 
        l;
  with e ->
    Printf.printf "File not found!";
    raise e;;
(* initialising the sheets for operations*)
let s:sheet = !(pick);;


(*for getting the first and second element of the pair*)
let fi (a,_) = a;;
let sec (_,a) = a;;

(*made it initially for debugging for generating arbitrary number of input*)
let rec initialize (s:sheet) : sheet = 
	let rec create a b = 
		if a < 1.0 then b
		else create (a -. 1.0) (
			let rec aux cpt acc =
			if cpt < 1.0 then acc
			else aux (cpt -. 1.0) ((a +. cpt ) :: acc)
				in aux 5.0 [] :: b)
	in create 5.0 [];;

(*for the required sub float list which is needed for computation of sub sheet for find_sub*)
let  find_x_d (s:sheet) (sto:float) (eno:float) :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			x :: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;

(*for adding each of the rows to form the matrix*)
let find_y (s:sheet) (stc:float) (enc:float) : sheet= 
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;



(*for finding the required sub heet for computation*)
let find_sub (s:sheet) (r:range) : sheet = 
 	find_y (find_x_d s (fi (fi r)) (fi (sec r))) (sec (fi r)) (sec (sec r));;

(*for counting the correct inputs in the sheet*)
let rec count_2 (lis:float list) =
	match lis with
	| [] -> 0
	| c :: d ->
	if c == nan then
	 (count_2 d)
	else
	1 + (count_2 d)
	 ;;

(*for counting the correct inouts in the whole sub sheet*)
let fill_count_value (s:sheet) (r:range) (i:index) : int =
	let p = find_sub s r in
		let rec count p c =
			match p with
			| [] -> 0
			| a :: b -> (count b c + (count_2 a))
		in count p 0;;


(*for filling the required count value to the sheet initially present*)
let rec fill_2 lis ed s r i= 
			 	match lis with
			 	| [] -> []
			 	| c :: d -> 
			 		if ed = 0.0 then
			 			((float)(fill_count_value s r i ) :: (fill_2 d (ed -. 1.0) s r i))
			 		else
			 			c ::  (fill_2 d (ed -. 1.0) s r i)


(*for filling the answer row wise to the matrix*)
let row_fill (s:sheet) (r:range) (i:index) : float list = 
	let p = find_sub s r in
		let rec count p =
			match p with
			| [] -> []
			| a :: b -> (float)(count_2 a) :: (count b)
		in count p ;;

(*for finding the ith element stored in the list*)
let find_x (v: float list) (a:float) : float  =

(let rec find_element myList k  = match myList with
        [] ->  0.0

        | ff_sec :: mac_sec -> 
        if  k = 0.0 then ff_sec
        else find_element mac_sec (k -. 1.0)


        in find_element v a 
      );;

(*for adding the required row  which is to be changed*)
let rec row_fill_2 lis ed s r i count =
		match lis with
		| [] -> []
		| c :: d -> 
			if ed =0.0 then 
				(find_x (row_fill s r i) count) :: (row_fill_2 d (ed -. 1.0) s r i count)
			else
				c :: (row_fill_2 d (ed -. 1.0) s r i count)


(*the parent method to be implemented in order ot find the full_count of the sheet*)
let fill_count (s:sheet) (r:range) (i:index)  :sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
			 (fill_2 a  en s r i) :: (fill b (st -. 1.0) (en))
			 	

			else
				a :: (fill b (st -. 1.0) (en ))
			in fill s sti eni;;

(*the parent method in order to fill the sheet by the rows of the matrix*)
let row_count (s:sheet) (r:range) (i:index) : sheet = 
	let sti = (fi i) in
	let eni = (sec i) in
	let cou = (row_fill s r i) in 
		let rec fill sp (st:float) (en:float) : sheet =
			match sp with
			| [] -> []
			| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(row_fill_2 a en s r i (-.(st))) :: (fill b (st -. 1.0 ) en )
			else
				a :: (fill b (st -. 1.0 ) en )
			in fill s sti eni;;

(*for finding a element int he list in order to converting the column based matrix to row based so as to do operations easier *)
let rec func mat (it:float) (fix:float) =
			match mat with
			| [] -> 0.0
			| a :: b -> 
				if it = fix then
				if a==nan then
					(func b (it +. 1.0) fix)
				else
					1.0 +. (func b (it +. 1.0) fix)
				else
					(func b (it +. 1.0) fix)
		;;



(*for finding one column of required sub matrix*)		
let rec iterc (a:sheet) (fix:float) =
	match a with
	|[] -> 0.0
	| b :: c -> (func b 0.0 fix) +. (iterc c fix);;

(*for making the whole column of elements a row based matrix*)
let rec fin_col (s:sheet) (a:float) =
	match a with
	| -1.0 -> []
	| _ ->  iterc s a :: (fin_col s (a -. 1.0));;

let lengh a = 
	match a with
	| [] -> 0.0
	| sx :: a -> (float)(length sx);;

(*for integrating the whole matrix into one sheet*)
let fill_vec_col (v : float list) (eni : float) (a:sheet) = 
	let pq = fin_col a ((lengh  a) -. 1.0) in
	let p = rev(pq) in
		let rec func1 ps vs enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x	p (-.(enis))) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 p v eni
					 ;;


(*parent function to add the modified sheet to the matrix*)
let col_count (s:sheet) (r:range) (i:index) : sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
				(fill_vec_col a en (find_sub s r)) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;

(*for adding the float whihc we have to add to each of the value in the particular range*)
let rec count_sum_2 (lis:float list) =
	match lis with
	| [] -> 0.0
	| c :: d -> c +. (count_sum_2 d) ;;


(*for updating the float list with the renewed element to be added to the list*)
let fill_count_value_sum (s:sheet) (r:range) (i:index) : float =
	let p = find_sub s r in
		let rec count p c =
			match p with
			| [] -> 0.0
			| a :: b -> (count b c +. (count_sum_2 a))
		in count p 0.0;;

(*for final adding of the modified float array to the modified sheet*)
let rec sum_2 lis ed s r i= 
			 	match lis with
			 	| [] -> []
			 	| c :: d -> 
			 		if ed = 0.0 then
			 			((fill_count_value_sum s r i ) :: (sum_2 d (ed -. 1.0) s r i))
			 		else
			 			c ::  (sum_2 d (ed -. 1.0) s r i)


(*the parent function for final addtion of each of the modified float list to the sheet*)
let full_sum (s:sheet) (r:range) (i:index)  :sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
			 (sum_2 a  en s r i) :: (fill b (st -. 1.0) (en))
			 	

			else
				a :: (fill b (st -. 1.0) (en))
			in fill s sti eni;;

(*for using the same earlier function to add each of the element and returning the modified float list to be added to the sheet*)
let row_fill_sum (s:sheet) (r:range) (i:index) : float list = 
	let p = find_sub s r in
		let rec count p =
			match p with
			| [] -> []
			| a :: b -> (count_sum_2 a) :: (count b)
		in count p ;;



(*for adding the float list to the final float list *)
let rec row_fill_sum_2 lis ed s r i count =
		match lis with
		| [] -> []
		| c :: d -> 
			if ed =0.0 then 
				(find_x (row_fill_sum s r i) count) :: (row_fill_sum_2 d (ed -. 1.0) s r i count)
			else
				c :: (row_fill_sum_2 d (ed -. 1.0) s r i count)




(*parent function for summing along the row*)
let row_sum (s:sheet) (r:range) (i:index) : sheet = 
	let sti = (fi i) in
	let eni = (sec i) in
	let cou = (row_fill_sum s r i) in 
		let rec fill sp (st:float) (en:float) : sheet =
			match sp with
			| [] -> []
			| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(row_fill_sum_2 a en s r i (-.(st))) :: (fill b (st -. 1.0 ) en )
			else
				a :: (fill b (st -. 1.0 ) en )
			in fill s sti eni;;


(*for summing up the each element for column sum*)
let rec func_sum mat (it:float) (fix:float) =
			match mat with
			| [] -> 0.0
			| a :: b -> 
				if it = fix then
					a +. (func_sum b (it +. 1.0) fix)
				else
					(func_sum b (it +. 1.0) fix)
		;;

(*for making the float list out of each columns*)
let rec iterc_sum (a:sheet) (fix:float) =
	match a with
	|[] -> 0.0
	| b :: c -> (func_sum b 0.0 fix) +. (iterc_sum c fix);;


(*for making the float list out of colums for the final float list*)
let rec fin_col_sum (s:sheet) (a:float) =
	match a with
	| -1.0 -> []
	| _ ->  iterc_sum s a :: (fin_col_sum s (a -. 1.0));;

let lengh_sum a = 
	match a with
	| [] -> 0.0
	| sx :: a -> (float)(length sx);;

(*for finding final sheet for the final sheet to be added to the modified list*)
let fill_vec_col_sum (v : float list) (eni : float) (a:sheet) = 
	let pq = fin_col_sum a ((lengh_sum  a) -. 1.0) in
	let p = rev(pq) in
		let rec func1 ps vs enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x	p (-.(enis))) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 p v eni
					 ;;


(*parent function for that to find col_sum*)
let col_sum (s:sheet) (r:range) (i:index) : sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
				(fill_vec_col_sum a en (find_sub s r)) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;

(*for summing up all the lement in the float list*)
let rec count_avg_2 (lis:float list) =
	match lis with
	| [] -> 0.0
	| c :: d -> c +. (count_avg_2 d) ;;

(*now add each summed up value of eah row*)
let avg_count_value (s:sheet) (r:range) (i:index) : float =
	let p = find_sub s r in
		let rec count p c =
			match p with
			| [] -> 0.0
			| a :: b -> (count b c +. (count_avg_2 a))
		in count p 0.0;;

(*dividing the value by the total count and adding it to the float list*)
let rec avg_2 lis ed s r i= 
			 	match lis with
			 	| [] -> []
			 	| c :: d -> 
			 		if ed = 0.0 then 
			 			(((avg_count_value s r i ) /. (float)(fill_count_value  s r i)):: (avg_2 d (ed -. 1.0) s r i))
			 		else
			 			c ::  (avg_2 d (ed -. 1.0) s r i)

(*finding the average of all the element in the given range and adding it to make a sheet*)

let full_avg (s:sheet) (r:range) (i:index)  :sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
			 (avg_2 a  en s r i) :: (fill b (st -. 1.0) (en))
			 	

			else
				a :: (fill b (st -. 1.0) (en ))
			in fill s sti eni;;

(*ading each of the element by first dividing it by length of the float list*)

let rec count_avg_2_r (lis:float list) (cons:float list) =
	match lis with
	| [] -> 0.0
	| c :: d -> (c /. (float)(length cons)) +. (count_avg_2_r d cons) ;;


(*adding element to float sheet*)
let row_fill_avg (s:sheet) (r:range) (i:index) : float list = 
	let p = find_sub s r in
		let rec count p =
			match p with
			| [] -> []
			| a :: b -> (count_avg_2_r a a) :: (count b)
		in count p ;;



(*adding element for row average*)
let rec row_fill_avg_2 lis ed s r i count =
		match lis with
		| [] -> []
		| c :: d -> 
			if ed =0.0 then 
				(find_x (row_fill_avg s r i) count) :: (row_fill_avg_2 d (ed -. 1.0) s r i count)
			else
				c :: (row_fill_avg_2 d (ed -. 1.0) s r i count)




(*parent function for finding row average*)
let row_avg (s:sheet) (r:range) (i:index) : sheet = 
	let sti = (fi i) in
	let eni = (sec i) in
	let cou = (row_fill s r i) in 
		let rec fill sp (st:float) (en:float) : sheet =
			match sp with
			| [] -> []
			| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(row_fill_avg_2 a en s r i (-.(st))) :: (fill b (st -. 1.0 ) en )
			else
				a :: (fill b (st -. 1.0 ) en )
			in fill s sti eni;;


(*sub function for column average*)
let rec func_avg mat (it:float) (fix:float) (s:sheet)=
			match mat with
			| [] -> 0.0
			| a :: b -> 
				if it = fix then
					(a /. (float)(length s)) +. (func_avg b (it +. 1.0) fix s)
				else
					(func_avg b (it +. 1.0) fix s)
		;;
(*sub function for column average*)
let rec iterc_avg (a:sheet) (fix:float) (d:sheet)=
	match a with
	|[] -> 0.0
	| b :: c -> (func_avg b 0.0 fix d) +. (iterc_avg c fix d);;

(*function for adding float list to sheet*)
let rec fin_col_avg (s:sheet) (a:float) =
	match a with
	| -1.0 -> []
	| _ ->  iterc_avg s a s:: (fin_col_avg s (a -. 1.0));;

(*finding length of avg float list*)
let lengh_avg a = 
	match a with
	| [] -> 0.0
	| sx :: a -> (float)(length sx);;

(*function for creating the modified float list*)
let fill_vec_col_avg (v : float list) (eni : float) (a:sheet) = 
	let pq = fin_col_avg a ((lengh_avg a) -. 1.0) in
	let p = rev(pq) in
		let rec func1 ps vs enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x	p (-.(enis))) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 p v eni
					 ;;


(*parent function for creating column average*)
let col_avg (s:sheet) (r:range) (i:index) : sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
				(fill_vec_col_avg a en (find_sub s r)) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;

(*fucntion for getting minimum of two numbers*)
let min (a:float) (b:float) =
	if a > b then b
	else a 

(*getting the minimum in a float list*)
let rec count_min_2 (lis:float list) =
	match lis with
	| [] -> max_float
	| c :: d -> min c (count_min_2 d)


(*getting the minimum count value of all the sheet*)
let min_count_value (s:sheet) (r:range) (i:index) : float =
	let p = find_sub s r in
		let rec count p c =
			match p with
			| [] -> max_float
			| a :: b -> (min (count b c)  (count_min_2 a))
		in count p 0.0;;

(*creating the modified list for array *)
let rec min_2 lis ed s r i= 
			 	match lis with
			 	| [] -> []
			 	| c :: d -> 
			 		if ed = 0.0 then 
			 			((min_count_value s r i ):: (min_2 d (ed -. 1.0) s r i))
			 		else
			 			c ::  (min_2 d (ed -. 1.0) s r i)


(*parent function for finding the minmum in the float list*)
let full_min (s:sheet) (r:range) (i:index)  :sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
			 (min_2 a  en s r i) :: (fill b (st -. 1.0) en )
			 	

			else
				a :: (fill b (st -. 1.0) en)
	
		in fill s sti eni;;






(*function for finding the minimum in the row*)
let row_fill_min (s:sheet) (r:range) (i:index) : float list = 
	let p = find_sub s r in
		let rec count p =
			match p with
			| [] -> []
			| a :: b -> (count_min_2 a) :: (count b)
		in count p ;;



(*function for adding the minimum to a float list*)
let rec row_fill_min_2 lis ed s r i count =
		match lis with
		| [] -> []
		| c :: d -> 
			if ed =0.0 then 
				(find_x (row_fill_min s r i) count) :: (row_fill_min_2 d (ed -. 1.0) s r i count)
			else
				c :: (row_fill_min_2 d (ed -. 1.0) s r i count)




(*parent function for filling up the sheet for row min*)
let row_min (s:sheet) (r:range) (i:index) : sheet = 
	let sti = (fi i) in
	let eni = (sec i) in
	let cou = (row_fill_min s r i) in 
		let rec fill sp (st:float) (en:float) : sheet =
			match sp with
			| [] -> []
			| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(row_fill_min_2 a en s r i (-.(st))) :: (fill b (st -. 1.0 ) en )
			else
				a :: (fill b (st -. 1.0 ) en )
			in fill s sti eni;;



(*sub function for finding the minimum element and then adding them to float list*)
let rec iterc_min (a:sheet) (fix:float) =
	match a with
	|[] -> []
	| b :: c ->  (find_x b fix) ::  (iterc_min c fix);;

(*sub function for finding minimum in the column*)
let min (a:sheet) (fix:float)=
	let r = iterc_min a fix in
	count_min_2  r;;


(*finding final column for minimum float list*)
let rec fin_col_min (s:sheet) (a:float) =
	match a with
	| -1.0 -> []
	| _ ->  min s a :: (fin_col_min s (a -. 1.0));;

(*finding the minimum in the list*)
let lengh_min a = 
	match a with
	| [] -> 0.0
	| sx :: a -> (float)(length sx);;

(* creating the float list consisting of the element which are minimum*)
let fill_vec_col_min (v : float list) (eni : float) (a:sheet) = 
	let pq = fin_col_min a ((lengh_min  a) -. 1.0) in
	let p = rev(pq) in
		let rec func1 ps vs enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x	p (-.(enis))) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 p v eni
					 ;;


(*parent function for finding the minimum*)
let col_min(s:sheet) (r:range) (i:index) : sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
				(fill_vec_col_min a en (find_sub s r)) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*fucntion for finding the maximum of two number*)
let max (a:float) (b:float) =
	if a > b then a
	else b 


(*fucntion for finding the maximum in the float list*)
let rec count_max_2 (lis:float list) =
	match lis with
	| [] -> min_float
	| c :: d -> max c (count_max_2 d)

(*function for finding the maximum throughout  the sheet*)
let max_count_value (s:sheet) (r:range) (i:index) : float =
	let p = find_sub s r in
		let rec count p c =
			match p with
			| [] -> min_float
			| a :: b -> (max (count b c)  (count_max_2 a))
		in count p 0.0;;

(*modifying the float list for adding the maximum value*)
let rec max_2 lis ed s r i= 
			 	match lis with
			 	| [] -> []
			 	| c :: d -> 
			 		if ed = 0.0 then 
			 			((max_count_value s r i ):: (max_2 d (ed -. 1.0) s r i))
			 		else
			 			c ::  (max_2 d (ed -. 1.0) s r i)


(*adding the modified float list to sheet as it is a parent fucntion*)
let full_max (s:sheet) (r:range) (i:index)  :sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
			 (max_2 a  en s r i) :: (fill b (st -. 1.0) en )
			 	

			else
				a :: (fill b (st -. 1.0) en)
	
		in fill s sti eni;;






(*sub function for finding the maximum in a  float list*)
let row_fill_max (s:sheet) (r:range) (i:index) : float list = 
	let p = find_sub s r in
		let rec count p =
			match p with
			| [] -> []
			| a :: b -> (count_max_2 a) :: (count b)
		in count p ;;



(*sub function for making the float list*)
let rec row_fill_max_2 lis ed s r i count =
		match lis with
		| [] -> []
		| c :: d -> 
			if ed =0.0 then 
				(find_x (row_fill_max s r i) count) :: (row_fill_max_2 d (ed -. 1.0) s r i count)
			else
				c :: (row_fill_max_2 d (ed -. 1.0) s r i count)




(*parent funciton for finding the maximum in a row and adding at the place near index*)
let row_max (s:sheet) (r:range) (i:index) : sheet = 
	let sti = (fi i) in
	let eni = (sec i) in
	let cou = (row_fill_max s r i) in 
		let rec fill sp (st:float) (en:float) : sheet =
			match sp with
			| [] -> []
			| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(row_fill_max_2 a en s r i (-.(st))) :: (fill b (st -. 1.0 ) en )
			else
				a :: (fill b (st -. 1.0 ) en )
			in fill s sti eni;;



(*final function just making up the substitute float list*)
let rec iterc_max (a:sheet) (fix:float) =
	match a with
	|[] -> []
	| b :: c ->  (find_x b fix) ::  (iterc_max c fix);;

(*finding the maimum in the float list*)
let max (a:sheet) (fix:float)=
	let r = iterc_max a fix in
	count_max_2  r;;


(*making a column out of the maximum so as to do it*)
let rec fin_col_max (s:sheet) (a:float) =
	match a with
	| -1.0 -> []
	| _ ->  max s a :: (fin_col_max s (a -. 1.0));;

(*length ofn the list to be considered*)
let lengh_max a = 
	match a with
	| [] -> 0.0
	| sx :: a -> (float)(length sx);;

(*finding the appropriate vec to find the column for column*)
let fill_vec_col_max (v : float list) (eni : float) (a:sheet) = 
	let pq = fin_col_max a ((lengh_max  a) -. 1.0) in
	let p = rev(pq) in
		let rec func1 ps vs enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x	p (-.(enis))) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 p v eni
					 ;;


(*parent function for finding the maximum in the column*)
let col_max(s:sheet) (r:range) (i:index) : sheet = 

	let sti = (fi i) in 
	let eni = (sec i) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st = 0.0 then
				(fill_vec_col_max a en (find_sub s r)) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*function for building up the float lis tof the added element*)
let  find_x_add (s:sheet) (sto:float) (eno:float)  :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			(x ):: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;

(*fucntion where eachof the elements are added*)
let find_y_add (s:sheet) (stc:float) (enc:float) (cons:float): sheet= 
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y +. cons :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;

(*finding the modified subsheet tobe placed based non the index*)
let find_sub_add (s:sheet) (r:range) (c:float) : sheet = 
 	find_y_add (find_x_add s (fi (fi r)  ) (fi (sec r))) (sec (fi r)) (sec (sec r)) c ;;

(*fidning the element in a sheet based on the x and y coordinate*)
let find_x_y (m: sheet) (a:float) (b:float) =


  (let r = m in
  let rec final_find r a b = 
 match r with                                                                  
      | [] ->  -1.0
      | x :: q ->

      begin
      if a = 0.0 then 


      
        (let rec find_element myList k  = match myList with
        [] ->  0.0

        | ff_sec :: mac_sec -> 
        if  k = 0.0 then ff_sec
        else find_element mac_sec (k -. 1.0)


        in find_element x b 
      )


    else final_find q (a -. 1.0) b 
	end
	in final_find r a b);;


(*finding the float list to be replaced int he sheet*)
let add_cons_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:float): float list =
	let ps = find_sub_add s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent function for adding the required sheet*)
let add_const (s:sheet) (r:range) (c:float) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(add_cons_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;



(*fucntion for building up the modified sheet for subtracted element*)
let  find_x_subt (s:sheet) (sto:float) (eno:float)  :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			(x ):: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;

(*function where actually subtraction is acting*)
let find_y_subt (s:sheet) (stc:float) (enc:float) (cons:float): sheet= 
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y -. cons :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;


(*finding the sub matrix which is to be replaced accoring to the sub matrix*)
let find_sub_subt (s:sheet) (r:range) (c:float) : sheet = 
 	find_y_subt (find_x_subt s (fi (fi r)  ) (fi (sec r))) (sec (fi r)) (sec (sec r)) c ;;





(*method to find the modified float list*)
let subt_cons_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:float): float list =
	let ps = find_sub_subt s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent function to subtract eachof the corresponding element in two sheets*)
let subt_const (s:sheet) (r:range) (c:float) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(subt_cons_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*function for making the final sheet which to be added on the parent sheet*)
let  find_x_mult (s:sheet) (sto:float) (eno:float)  :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			(x ):: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;

(*function where actual multiplicaiton occurs with the constant given*)
let find_y_mult (s:sheet) (stc:float) (enc:float) (cons:float): sheet= 
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y *. cons :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;


(*finding the sub matrix from the sheet*)
let find_sub_mult (s:sheet) (r:range) (c:float) : sheet = 
 	find_y_mult (find_x_mult s (fi (fi r)  ) (fi (sec r))) (sec (fi r)) (sec (sec r)) c ;;




(*helper funciton for replacing the float list with the modified one where ever necessary*)
let mult_cons_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:float): float list =
	let ps = find_sub_mult s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent function for carring out the multiplication with a constnat as given*)
let mult_const (s:sheet) (r:range) (c:float) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(mult_cons_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*fucntion to find the final sheet obtained by dividing the required sheet y the constant provided*)
let  find_x_div (s:sheet) (sto:float) (eno:float)  :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			(x ):: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;

(*fucnton where actual dividsion occurs which the constant provided as the input*)
let find_y_div (s:sheet) (stc:float) (enc:float) (cons:float): sheet= 
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y /. cons :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;


(*function for finding required sheet to be added to the final sheet*)
let find_sub_div (s:sheet) (r:range) (c:float) : sheet = 
 	find_y_div (find_x_div s (fi (fi r)  ) (fi (sec r))) (sec (fi r)) (sec (sec r)) c ;;





(*funciton for fining the constant division*)
let div_cons_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:float): float list =
	let ps = find_sub_div s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent funciton for finding the required sheet consisting of the required elementsn obtaibde afetr divin gthem by the constant provided*)
let div_const (s:sheet) (r:range) (c:float) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(div_cons_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;

(*adding two sheets and then returning the added matrix*)
let rec addm (m1:sheet) (m2:sheet): sheet =

  
let fi = m1 in 
	let se = m2 in 
		let rec listadd a b =                                                         
			match a with                                                                  
			  | [] -> b
			  | g :: m -> (match b with
   			  | h :: n -> (
   						let rec listadd a b =                                                         
						match a with                                                                  
						  | [] -> b
						  | g :: m -> (match b with
   						  | h :: n -> (g +. h) :: (listadd m n)
    					  | [] -> a
    					)
    					 in listadd g h) :: (listadd m n)
    			 		| [] -> a
    					)
    					 in listadd fi se 
    	;; 


(*finding the ouput returned added matrix whichi is ti be replaced in the sheet*)
let find_sub_add_range (s:sheet) (r:range) (c:range) : sheet = 
 	addm (find_sub s r) (find_sub s c)





(*funciton for generating the float list which is to be replace din the sheet*)
let add_range_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:range): float list =
	let ps = find_sub_add_range s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent funciton for adding two ranges at once*)
let add_range (s:sheet) (r:range) (c:range) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(add_range_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;

(*fucnton for returning the subtraction of tow sheet subtract3ed element by element*)
let rec subm (m1:sheet) (m2:sheet): sheet =

  
let fi = m1 in 
	let se = m2 in 
		let rec listadd a b =                                                         
			match a with                                                                  
			  | [] -> b
			  | g :: m -> (match b with
   			  | h :: n -> (
   						let rec listadd a b =                                                         
						match a with                                                                  
						  | [] -> b
						  | g :: m -> (match b with
   						  | h :: n -> (g -. h) :: (listadd m n)
    					  | [] -> a
    					)
    					 in listadd g h) :: (listadd m n)
    			 		| [] -> a
    					)
    					 in listadd fi se 
    	;; 

(*fucntionfor returning the final sheet which to be replaced in the sheet *)
let find_sub_sub_range (s:sheet) (r:range) (c:range) : sheet = 
 	subm (find_sub s r) (find_sub s c)





(*fucntion for finding the list whici to be added to the final returnnin sheet*)
let sub_range_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:range): float list =
	let ps = find_sub_sub_range s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent fucntion for finding the result of the element by element subtrated given by the elements obtainde dby subtrating eacho fthe element fom the range one to one*)
let subt_range (s:sheet) (r:range) (c:range) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(sub_range_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*function for multiplyingt he two adjoing element of two sheet one on one*)
let rec mulm (m1:sheet) (m2:sheet): sheet =

  
let fi = m1 in 
	let se = m2 in 
		let rec listadd a b =                                                         
			match a with                                                                  
			  | [] -> b
			  | g :: m -> (match b with
   			  | h :: n -> (
   						let rec listadd a b =                                                         
						match a with                                                                  
						  | [] -> b
						  | g :: m -> (match b with
   						  | h :: n -> (g *. h) :: (listadd m n)
    					  | [] -> a
    					)
    					 in listadd g h) :: (listadd m n)
    			 		| [] -> a
    					)
    					 in listadd fi se 
    	;; 

(*finding the final sub matrix to be replaced in the sheet so as to finding the required sub sheet to be replaced*)
let find_sub_mult_range (s:sheet) (r:range) (c:range) : sheet = 
 	mulm (find_sub s r) (find_sub s c)





(*function for finding the required ist to be replaced in the original sheet accoring to the index provided*)
let mult_range_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:range): float list =
	let ps = find_sub_mult_range s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent fucntion for finding the final sheet after multiplying the element in each of the ranges one by one*)
let mult_range (s:sheet) (r:range) (c:range) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(mult_range_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*element obtained by dividing each of the element obtained by dividing them one on one*)
let rec divm (m1:sheet) (m2:sheet): sheet =

  
let fi = m1 in 
	let se = m2 in 
		let rec listadd a b =                                                         
			match a with                                                                  
			  | [] -> b
			  | g :: m -> (match b with
   			  | h :: n -> (
   						let rec listadd a b =                                                         
						match a with                                                                  
						  | [] -> b
						  | g :: m -> (match b with
   						  | h :: n -> (g /. h) :: (listadd m n)
    					  | [] -> a
    					)
    					 in listadd g h) :: (listadd m n)
    			 		| [] -> a
    					)
    					 in listadd fi se 
    	;; 


(*element obtained by fidning the sub sheet for the above ooperation*)
let find_sub_div_range (s:sheet) (r:range) (c:range) : sheet = 
 	divm (find_sub s r) (find_sub s c)





(*fucntionf or making the modified floa tlist for adding them to the sheet*)
let div_range_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:range): float list =
	let ps = find_sub_div_range s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;


(*parent function for making the function for dividing over range*)
let div_range (s:sheet) (r:range) (c:range) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(div_range_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*adding the respctive element of two list for final float list list*)
let  find_x_addi (s:sheet) (sto:float) (eno:float)   :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			(x ):: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;

(*main funciton where actually is adding is occuring*)
let find_y_addi (s:sheet) (stc:float) (enc:float) (cons:index) (org:sheet): sheet= 

	let sti = (fi cons) in 
	let eni = (sec cons) in
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y +. (find_x_y org sti eni) :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;

(*fucntion for finding final sub matrix*)
let find_sub_addi (s:sheet) (r:range) (c:index) : sheet = 
 	find_y_addi (find_x_addi s (fi (fi r)  ) (fi (sec r))) (sec (fi r)) (sec (sec r)) c s ;;


(*funciton for making the final float list to be added to the sheet*)
let addi_cons_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:index): float list =
	let ps = find_sub_addi s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*function for adding the constant ot the selected part of the sheet as shown accoring to range and index*)
let addi_const (s:sheet) (r:range) (c:index) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(addi_cons_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;



(*fucnton for finding the float list to be added to the sheet*)
let  find_x_subi (s:sheet) (sto:float) (eno:float)   :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			(x ):: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;


(*fucntion where the actual fucntion operation is actually occuring*)
let find_y_subi (s:sheet) (stc:float) (enc:float) (cons:index) (org:sheet): sheet= 

	let sti = (fi cons) in 
	let eni = (sec cons) in
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y -. (find_x_y org sti eni) :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;


(*function for finding the subsheet for the operation *)
let find_sub_subi (s:sheet) (r:range) (c:index) : sheet = 
 	find_y_subi (find_x_subi s (fi (fi r)  ) (fi (sec r))) (sec (fi r)) (sec (sec r)) c s ;;


(*fucntion for finding the new float list to be replaced in the original sheet accordance with subtraction*)
let subi_cons_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:index): float list =
	let ps = find_sub_subi s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent fucntion for subtrating all the element in the sheet divided by the range by the element represented bythe element represented b the index on the sheet*)
let subi_const (s:sheet) (r:range) (c:index) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(subi_cons_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*fucntion for appending the float list one to one for making the final sheet whichi to be replaced in the original sheet*)
let  find_x_multi (s:sheet) (sto:float) (eno:float)   :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			(x ):: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;

(*fucntion for finding the the elements are replaced*)
let find_y_multi (s:sheet) (stc:float) (enc:float) (cons:index) (org:sheet): sheet= 

	let sti = (fi cons) in 
	let eni = (sec cons) in
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y *. (find_x_y org sti eni) :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;

(*function for finding the sub sheet*)
let find_sub_multi (s:sheet) (r:range) (c:index) : sheet = 
 	find_y_multi (find_x_multi s (fi (fi r)  ) (fi (sec r))) (sec (fi r)) (sec (sec r)) c s ;;


(*function for finding the float list which is to be replaced by the sheet in the sheet*)
let multi_cons_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:index): float list =
	let ps = find_sub_multi s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent funciton for multipluying all the lement in the range by the lement loacted at the index*)
let multi_const (s:sheet) (r:range) (c:index) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(multi_cons_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;


(*sub funciton for division for finding the float list*)
let  find_x_divi (s:sheet) (sto:float) (eno:float)   :sheet  = 
	let rec find_final r a b =
		match r with 
		| [] -> []
		| x:: q -> 
		if (a <= 0.0) && (b >= 0.0) then
			(x ):: (find_final q (a -. 1.0) (b -. 1.0) )
		else
			(find_final q (a -. 1.0) (b -. 1.0))
		in find_final s sto eno;;

(*sub function where the element by element divison occurs*)
let find_y_divi (s:sheet) (stc:float) (enc:float) (cons:index) (org:sheet): sheet= 

	let sti = (fi cons) in 
	let eni = (sec cons) in
	let rec find_final r a b =
		match r with
		| [] -> []
		| x :: q -> 
		(
			let rec find_net r a b : float list = 
				match r with
				| [] -> []
				| y :: t ->
					if (a <= 0.0 ) && (b >= 0.0) then
						y /. (find_x_y org sti eni) :: (find_net t (a -. 1.0) (b -. 1.0) )
					else
						(find_net t (a -. 1.0) (b -. 1.0))
					in find_net x a b
		) :: (find_final q a b)
	in find_final s stc enc;;

(*funciton for finding the sub sheet*)
let find_sub_divi (s:sheet) (r:range) (c:index) : sheet = 
 	find_y_divi (find_x_divi s (fi (fi r)  ) (fi (sec r))) (sec (fi r)) (sec (sec r)) c s ;;


(*helper fucniton for finding the float list which is to be replaced int he float list*)
let divi_cons_helper (s:sheet) (r:range) (sti:float) (eni:float) (st:float) (v:float list) (c:index): float list =
	let ps = find_sub_divi s r c in
		let rec func1 ps (vs:float list) enis : float list =
			match vs with
			| [] -> []
			| asd :: bs ->
				
					if enis <= 0.0 && enis >( -.(float) (length ps)) then
					(find_x_y	ps (-.(enis)) st) :: (func1 ps bs (enis -. 1.0))
				else
					 asd :: (func1 ps bs (enis -. 1.0))
					in func1 ps v eni
					 ;;

(*parent funciton for dividing the element  in the range by element at the index*)
let divi_const (s:sheet) (r:range) (c:index) (i:index) : sheet =

	let sti = (fi i) in 
	let eni = (sec i) in
	let cou = (find_sub s r) in
	let rec fill sp (st:float) (en:float) : sheet = 
		match sp with 
		| [] -> []
		| a :: b ->
			if st <= 0.0 && st > -. (float)(length cou) then
				(divi_cons_helper s r sti eni (-. st) a  c) :: (fill b (st -. 1.0) en )

			else
				a :: (fill b (st -. 1.0) en )
			in fill s sti eni;;











(*functions for printing the ouput sheet*)
let string_of_float_list l = String.concat " " (List.map string_of_float l)

let float_list_to_string l = String.concat "\n" (List.map string_of_float_list l)

  


%}
%token ADD SUBT MULT DIV COUNT ROWCOUNT COLCOUNT SUM ROWSUM COLSUM AVG ROWAVG 
%token NEWLINE COLAVG MIN ROWMIN COLMIN MAX ROWMAX COLMAX ASSIGNMENT SEMICOLON 
%token OPENSQUAREBRACKETS CLOSESQUAREBRACKETS OPENCURVEBRACKETS CLOSECURVEBRACKETS
%token COMMA COLON CHAR
%token <float> NUM
%start input
%type <unit> input
%% /* Grammar rules and actions follow */
input:
/* empty */ { }
| input exp { printf "%s" (float_list_to_string $2)}
;
exp: indices ASSIGNMENT COUNT range     {fill_count s ($4) ($1)}
| indices ASSIGNMENT ROWCOUNT range     {row_count s ($4) ($1)}
| indices ASSIGNMENT COLCOUNT range     {col_count s ($4) ($1)}
| indices ASSIGNMENT SUM range          {full_sum s ($4) ($1)}
| indices ASSIGNMENT ROWSUM range       {row_sum s ($4) ($1)}
| indices ASSIGNMENT COLSUM range       {col_sum s ($4) ($1)}
| indices ASSIGNMENT AVG range          {full_avg s ($4) ($1)}
| indices ASSIGNMENT ROWAVG range       {row_avg s ($4) ($1)}
| indices ASSIGNMENT COLAVG range       {col_avg s ($4) ($1)}
| indices ASSIGNMENT MIN range          {full_min s ($4) ($1)}
| indices ASSIGNMENT ROWMIN range       {row_min s ($4) ($1)}
| indices ASSIGNMENT COLMIN range       {col_min s ($4) ($1)}
| indices ASSIGNMENT MAX range          {full_max s ($4) ($1)}
| indices ASSIGNMENT ROWMAX range       {row_max s ($4) ($1)}
| indices ASSIGNMENT COLMAX range       {col_max s ($4) ($1)}
| indices ASSIGNMENT ADD range range    {add_range s ($4) ($5) ($1)} 
| indices ASSIGNMENT SUBT range range   {subt_range s ($4) ($5) ($1)}
| indices ASSIGNMENT MULT range range   {mult_range s ($4) ($5) ($1)}
| indices ASSIGNMENT DIV range range    {div_range s ($4) ($5) ($1)}
| indices ASSIGNMENT ADD NUM range    {add_const s ($5) ($4) ($1)}
| indices ASSIGNMENT SUBT NUM range   {subt_const s ($5) ($4) ($1)}
| indices ASSIGNMENT MULT NUM range   {mult_const s ($5) ($4) ($1)}
| indices ASSIGNMENT DIV NUM range    {div_const s ($5) ($4) ($1)}
| indices ASSIGNMENT ADD range NUM    {add_const s ($4) ($5) ($1)}
| indices ASSIGNMENT SUBT range NUM   {subt_const s ($4) ($5) ($1)}
| indices ASSIGNMENT MULT range NUM   {mult_const s ($4) ($5) ($1)}
| indices ASSIGNMENT DIV range NUM    {div_const s ($4) ($5) ($1)}
| indices ASSIGNMENT ADD indices range  {addi_const s ($5) ($4) ($1)}
| indices ASSIGNMENT SUBT indices range {subi_const s ($5) ($4) ($1)}
| indices ASSIGNMENT MULT indices range {multi_const s ($5) ($4) ($1)}
| indices ASSIGNMENT DIV indices range  {divi_const s ($5) ($4) ($1)}
| indices ASSIGNMENT ADD range indices  {addi_const s ($4) ($5) ($1)}
| indices ASSIGNMENT SUBT range indices {subi_const s ($4) ($5) ($1)}
| indices ASSIGNMENT MULT range indices {multi_const s ($4) ($5) ($1)}
| indices ASSIGNMENT DIV range indices  {divi_const s ($4) ($5) ($1)}
;


range: OPENCURVEBRACKETS indices COLON indices CLOSECURVEBRACKETS {($2,$4)}
;


indices: OPENSQUAREBRACKETS NUM COMMA NUM CLOSESQUAREBRACKETS {($2,$4) }
;



%%


let parse_error s = (* Called by the parser function on error *)
print_endline s;
flush stdout

