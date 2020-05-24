#load "str.cma";;


let m = 6;; 
let n = 6;;

let rec print_list stry : float list =  
match stry with
[] ->  []
| e::l -> 

if e = "" then 
nan :: (print_list l) 
else
float_of_string e :: (print_list l)


let rec print_list_f = function 
[] -> ()
| e::l -> Printf.printf "%f" e ; print_string " "; print_list_f l


let string_of_float_list l = String.concat " " (List.map string_of_float l);;

let float_list_to_string l = String.concat "\n" (List.map string_of_float_list l);;


let pick =
  try
  let  l = ref [] in
    let in_stream = open_in "sheet.csv" in
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

let s:sheet = !(pick)