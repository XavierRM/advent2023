open List
open Printf

let file = "input.txt"

let int_value c = int_of_char c - int_of_char '0'
let lineNum digitList =
  if (length digitList) == 1 then
    (nth digitList 0)*10 + (nth digitList 0) 
  else 
    (nth digitList 0) + 10*(nth digitList ((length digitList)-1))

let parseLine line = 
  let rec auxParseLine line nnumbers acum =
    let currentChar = line.[0] in
      match currentChar, String.length line with
      | '0'..'9', 1 -> (int_value currentChar)::acum
      | _, 1-> acum
      | '0'..'9', _ -> auxParseLine (String.sub line 1 ((String.length line)-1)) (nnumbers/10) ((int_value currentChar)::acum) 
      | _, _ -> auxParseLine (String.sub line 1 ((String.length line)-1)) nnumbers acum
  in auxParseLine line 10 [];;

let rec auxParse sum file =
  match input_line file with
    | line -> auxParse (sum + (lineNum (parseLine line))) file
    | exception End_of_file -> sum

let parseFile file = auxParse 0 file;;

let () = printf "%i\n" (parseFile (open_in file))