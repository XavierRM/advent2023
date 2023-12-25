let file = "day-two/input2Challenge1.txt"

let colorBasedComparison currentColor currentValue red green blue = 
  match currentColor with
    | "red" -> if (currentValue >= !red) then red := currentValue else ()
    | "green" -> if (currentValue >= !green) then green := currentValue else ()
    | "blue" -> if (currentValue >= !blue) then blue := currentValue else ()
    | _ -> ()

let joinListValues inputList =
  let rec auxJoinValues inputList accum =
    match inputList with
    | [] -> accum
    | hd::tl -> auxJoinValues tl (List.append hd accum)
  in auxJoinValues inputList []  

let compareSet currentSet red green blue =
  let rec auxCompare currentSet =
    match currentSet with
    | [] -> ()
    | _::[] -> ()
    | first::second::tl -> colorBasedComparison second (int_of_string first) red green blue; auxCompare tl
  in auxCompare currentSet

let getCubeValues line =
  let stringSets auxLine = String.split_on_char ';' auxLine in
  let valuesList stringSet = List.map String.trim (String.split_on_char ',' stringSet) |> (List.map (String.split_on_char ' ')) |> joinListValues in
  let red = ref 1 in let green = ref 1 in let blue = ref 1 in
  let rec auxGetHighestValues auxStringSets =
    match auxStringSets with
    | [] -> !red * !green * !blue
    | hd::tl -> compareSet (valuesList hd) red green blue; auxGetHighestValues tl
  in auxGetHighestValues (stringSets (List.nth (String.split_on_char ':' line) 1))

let parseLine line =
  getCubeValues line

let parseFile file =
  let rec auxParse sum file =
    match input_line file with
      | line -> auxParse (sum + (parseLine line)) file
      | exception End_of_file -> sum
  in auxParse 0 file

  let () = Printf.printf "%i\n" (parseFile (open_in file))