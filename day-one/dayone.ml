open List
open Printf

let file = "day-one/inputChallenge2.txt"

let int_value c = int_of_char c - int_of_char '0';;
let lineNum digitList =
  if ((length digitList) == 0) then
    0
  else
    match (nth digitList 0), (nth digitList ((length digitList)-1)) with
      | Some x, Some y -> x + y*10
      | _, _ -> 0

(* --------------------------------------------------------------------------------------------------------------------------- *)

(* Commenting to avoid unused warning *)
(* let parseLinePartOne line = 
  let rec auxParseLine line acum =
    let currentChar = line.[0] in
      match currentChar, String.length line with
      | '0'..'9', 1 -> (Some (int_value currentChar))::acum
      | _, 1-> acum
      | '0'..'9', _ -> auxParseLine (String.sub line 1 ((String.length line)-1)) ((Some (int_value currentChar))::acum) 
      | _, _ -> auxParseLine (String.sub line 1 ((String.length line)-1)) acum
  in auxParseLine line [];;

let rec auxParse sum file =
  match input_line file with
    | line -> auxParse (sum + (lineNum (parseLinePartOne line))) file
    | exception End_of_file -> sum
 *)

(* let parseFilePartOne file = auxParse 0 file;; *)

(* --------------------------------------------------------------------------------------------------------------------------- *)

let getNumber input =
  if (String.starts_with ~prefix:"one" input) then
    Some "one"
  else
  if (String.starts_with ~prefix:"two" input) then
    Some "two"
  else
  if (String.starts_with ~prefix:"three" input) then
    Some "three"
  else
  if (String.starts_with ~prefix:"four" input) then
    Some "four"
  else
  if (String.starts_with ~prefix:"five" input) then
    Some "five"
  else
  if (String.starts_with ~prefix:"six" input) then
    Some "six"
  else
  if (String.starts_with ~prefix:"seven" input) then
    Some "seven"
  else
  if (String.starts_with ~prefix:"eight" input) then
    Some "eight"
  else
  if (String.starts_with ~prefix:"nine" input) then
    Some "nine"
  else
    None

let convertNumber stringNumber =
  match stringNumber with
  | "one" -> Some 1
  | "two" -> Some 2
  | "three" -> Some 3
  | "four" -> Some 4
  | "five" -> Some 5
  | "six" -> Some 6
  | "seven" -> Some 7
  | "eight" -> Some 8
  | "nine" -> Some 9
  | _ -> None

let parseLinePartTwo line = 
  let rec auxParseLine stringLine acum =
    match (getNumber stringLine), (String.length stringLine) with
    | _, 0 -> acum
    | None, _ -> 
      (match stringLine.[0] with
      | '0'..'9' -> auxParseLine (String.sub stringLine 1 ((String.length stringLine)-1)) ((Some (int_value stringLine.[0]))::acum)
      | _ -> auxParseLine (String.sub stringLine 1 ((String.length stringLine)-1)) acum)
    | Some x, _ -> 
      let subLength = (String.length x)-1 in auxParseLine (String.sub stringLine subLength ((String.length stringLine)-subLength)) ((convertNumber x)::acum)
  in auxParseLine line [];;
    

let rec auxParse sum file =
  match input_line file with
    | line -> 
      auxParse (sum + (lineNum (parseLinePartTwo line))) file
    | exception End_of_file -> sum

let parseFilePartTwo file = auxParse 0 file;;

let () = printf "%i\n" (parseFilePartTwo (open_in file))