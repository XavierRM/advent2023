let file = "day-two/input2Challenge1.txt"

let rgbValues = [12;13;14]

let getFirst list = 
  List.nth list 0
(* let charIntValue c = int_of_char c - int_of_char '0';; *)

let getColorPos color = 
  match color with
    | "red" -> 0
    | "green" -> 1
    | "blue" -> 2
    | _ -> -1

let getGameId line =
  let gameIdWord = 
    (String.split_on_char ':' line) |> getFirst
  in int_of_string (List.nth (String.split_on_char ' ' gameIdWord) 1)

let joinListValues inputList =
  let rec auxJoinValues inputList accum =
    match inputList with
    | [] -> accum
    | hd::tl -> auxJoinValues tl (List.append hd accum)
  in auxJoinValues inputList []  

let compareSet currentSet highestValues =
  let rec auxCompare currentSet isValid =
    match currentSet, isValid with
    | _, false -> 
      isValid
    | [], _ -> isValid
    | _::[], _ -> isValid
    | first::second::tl, _ ->
      auxCompare tl (first |> int_of_string <= (List.nth highestValues (second |> getColorPos)))
  in auxCompare currentSet true

let getCubeValues line =
  let stringSets auxLine = String.split_on_char ';' auxLine in
  let valuesList stringSet = List.map String.trim (String.split_on_char ',' stringSet) |> (List.map (String.split_on_char ' ')) |> joinListValues in
  let rec auxGetHighestValues auxStringSets isValidLine =
    match auxStringSets, isValidLine with
    | _, false -> isValidLine
    | [], _ -> isValidLine
    | hd::tl, _ -> auxGetHighestValues tl (compareSet (valuesList hd) rgbValues)
  in auxGetHighestValues (stringSets (List.nth (String.split_on_char ':' line) 1)) true

let parseLine line =
  let gameId = getGameId line in
  let isLineValid = getCubeValues line in
  match isLineValid with
  | true -> gameId
  | false -> 0

let rec auxParse sum file =
  match input_line file with
    | line -> auxParse (sum + (parseLine line)) file
    | exception End_of_file -> sum

  let parseFile file = auxParse 0 file;;

  let () = Printf.printf "%i\n" (parseFile (open_in file))