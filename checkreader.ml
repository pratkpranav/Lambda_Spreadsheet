#load "str.cma";;

let m = 6;; 
let n = 6;;

let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string " "; print_list l


let string_of_float_list l = String.concat " " (List.map string_of_float l)


let _ =
  try
    let in_stream = open_in "sheet.csv" in
        for i=0 to (m-1) do
          let line = input_line in_stream in
          Printf.printf "%s" (string_of_float_list ( List.map float_of_string (Str.split (Str.regexp "[^0-9]+") line)));
          Printf.printf "\n";
        done;  
        close_in in_stream; 
  with e ->
    Printf.printf "File not found!";
    raise e