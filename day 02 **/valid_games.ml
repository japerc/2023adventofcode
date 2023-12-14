let rec read_ic_by_line open_file =
  match In_channel.input_line open_file with
  | None -> []
  | Some s -> s :: read_ic_by_line(open_file)


let input typ = 
  if typ = "puzzle" then
    let path = "/Users/jakab/adventofcode/day 02/puzzle_input" in
    In_channel.with_open_bin path read_ic_by_line
  else if typ = "test" then
    In_channel.with_open_bin "/Users/jakab/adventofcode/day 02/test_input" read_ic_by_line
  else
    raise (Invalid_argument "type of data requested unavailable")


let format_draws draws = 
    let split_1 str = String.split_on_char ';' str in
    let split_2 lst = List.map (fun str -> String.split_on_char ',' str) lst in
    let trim_all lst_lst = List.map (fun lst -> List.map String.trim lst) lst_lst in
    draws |> split_1 |> split_2 |> trim_all  


let is_valid_draw draw = 
  
  let check_color str = 
    match (String.split_on_char ' ' str) with
    | [n; "red"] -> (int_of_string n) <= 12
    | [n; "green"] -> (int_of_string n) <= 13
    | [n; "blue"] -> (int_of_string n) <= 14
    | _ -> false in
  
  List.for_all check_color draw


let is_valid_game draws = 
  
  draws |> format_draws |> List.for_all is_valid_draw


let get_game_nr str = int_of_string (List.nth (String.split_on_char ' ' str) 1)


let add_valid_game (total_score: int) (game: string): int =
  
  let game_number, game_valid = match (String.split_on_char ':' game) with
  | [a; b] -> get_game_nr a, is_valid_game b
  | _ -> 0, false in
  
  if game_valid then
    total_score + game_number
  else
    total_score


let sum_valid_games all_lines = List.fold_left add_valid_game 0 all_lines


(*PART TWO*)


type cubes = {red: int; green: int; blue: int}


let min_colors_draw cbs draw =
  
  let min_color cbs str = 
    match (String.split_on_char ' ' str) with
    | [n; "red"] when int_of_string n > cbs.red -> {cbs with red = int_of_string n}
    | [n; "green"] when int_of_string n > cbs.green -> {cbs with green = int_of_string n}
    | [n; "blue"] when int_of_string n > cbs.blue -> {cbs with blue = int_of_string n }
    | [_; "red"|"green"|"blue"] -> cbs
    | _ -> raise (Invalid_argument "Invalid cube draw") in

  List.fold_left min_color cbs draw


let power_of_game draws = 

  let min_cubes = draws |> format_draws |> 
  List.fold_left min_colors_draw {red = 0; green = 0; blue = 0} in
  min_cubes.red * min_cubes.green * min_cubes.blue


let add_game_pwr total_score game = 
  let draws = List.nth (String.split_on_char ':' game) 1 in
  total_score + power_of_game draws
  

let sum_powers all_lines = List.fold_left add_game_pwr 0 all_lines
