let file = "day-three/input2Challenge1.txt"

let lineSize = ref 0

let int_value c = int_of_char c - int_of_char '0';;

let getNum charList index =
  match (List.nth charList index), (List.nth charList (index+1)), (List.nth charList (index+2)) with
  | first, second, third -> 
    match first, second, third with
    | '0'..'9', '0'..'9', '0'..'9' -> (((int_value first)*100) + ((int_value second)*10) + (int_value third), 3)
    | '0'..'9', '0'..'9', ('.'|'*'|'/'|'%'|'+'|'#'|'@'|'='|'&') -> (((int_value first)*10) + (int_value second), 2)
    | '0'..'9', ('.'|'*'|'/'|'%'|'+'|'#'|'@'|'='|'&'), ('.'|'*'|'/'|'%'|'+'|'#'|'@'|'='|'&') -> (int_value first, 1)
    | _, _, _ -> (0, 0)

let getAdjPositions index numSize = 
  match numSize with
  | 1 -> [index-(!lineSize)-1;index-(!lineSize);index-(!lineSize)+1;index-1;index+1;index+(!lineSize)-1;index+(!lineSize);index+(!lineSize)+1]
  | 2 -> [index-(!lineSize)-1;index-(!lineSize);index-(!lineSize)+1;index-(!lineSize)+2;index-1;index+2;index+(!lineSize)-1;index+(!lineSize);index+(!lineSize)+1;index+(!lineSize)+2]
  | 3 -> [index-(!lineSize)-1;index-(!lineSize);index-(!lineSize)+1;index-(!lineSize)+2;index-(!lineSize)+3;index-1;index+3;index+(!lineSize)-1;index+(!lineSize);index+(!lineSize)+1;index+(!lineSize)+2;index+(!lineSize)+3]
  | _ -> []

let checkAdjSymbols charList positions = 
  let rec process charList positions isAdjSymbol =
    match positions, isAdjSymbol with
    | _, true -> isAdjSymbol
    | [], _ -> isAdjSymbol 
    | hd::tl, _ -> 
      if (hd >= 0) then
        match (List.nth charList hd) with
          | ('*'|'/'|'%'|'+'|'#'|'@'|'='|'&') -> process charList tl true
          | _ -> process charList tl false
      else
        process charList tl isAdjSymbol
  in process charList positions false

let file_to_charList file =
  let rec string_to_charList file acumList =
    match input_line file with
    | line -> 
      lineSize := String.length line;
      string_to_charList file (List.rev_append (line |> String.to_seq |> List.of_seq) acumList)
    | exception End_of_file -> acumList
  in List.rev (string_to_charList file [])
let parse charList =
  let rec auxParseLine charList index acum =
    match (List.nth charList index) with
      | '0'..'9' -> 
        let (num, numSize) = getNum charList index in
        if (checkAdjSymbols charList (getAdjPositions index numSize)) then (auxParseLine charList (index+numSize) (acum+num)) else (auxParseLine charList (index+numSize) acum)
      | _ -> (auxParseLine charList (index+1) acum)
      | exception Failure _ -> acum
  in auxParseLine charList 0 0


let parseFile file : int =
  file |> file_to_charList |> parse

let () = Printf.printf "%i\n" (parseFile (open_in file))