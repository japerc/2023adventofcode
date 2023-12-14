let rec read_file open_file =
  match input_line open_file with
  | "END" -> []
  | s -> s :: read_file(open_file)

type record = {
  digits: char * char; 
  last_five: char * char * char * char * char}

let parse_char (acc: record) (c : char) : record = 

  let new_five = match acc.last_five with
  | (_, a2, a3, a4, a5) -> (a2, a3, a4, a5, c) in

  let is_digit, c' = match new_five with
  | (_, _, 'o', 'n', 'e') -> true,'1'
  | (_, _, 't', 'w', 'o') -> true,'2'
  | ('t', 'h', 'r', 'e', 'e') -> true,'3'
  | (_, 'f', 'o', 'u', 'r') -> true,'4'
  | (_, 'f', 'i', 'v', 'e') -> true,'5'
  | (_, _, 's', 'i', 'x') -> true,'6'
  | ('s', 'e', 'v', 'e', 'n') -> true,'7'
  | ('e', 'i', 'g', 'h', 't') -> true,'8'
  | (_, 'n', 'i', 'n', 'e') -> true,'9'
  | _ -> (c > '0' && c <= '9'), c in

  match acc.digits with
  | ('*', '*') when is_digit -> 
    {digits = (c', c'); 
    last_five = ('*', '*', '*', '*', '*')}
  | _ when is_digit -> 
    {digits = (fst acc.digits, c'); 
    last_five = ('*', '*', '*', '*', '*')}
  | _ -> {digits = acc.digits; last_five = new_five}

let get_digit_tuple (str: string) : record = 
  String.fold_left parse_char {
    digits = ('*', '*'); 
    last_five = ('*', '*', '*', '*', '*')} str

let int_from_tuple (tuple: char * char) : int = 
  int_of_string (Char.escaped (fst tuple) ^ Char.escaped (snd tuple))

let parse_line (acc: int) (str: string) : int = 
  acc + int_from_tuple (get_digit_tuple str).digits

let parse_document (lst: string list) : int = 
  List.fold_left parse_line 0 lst

let sum_of_my_calibration_values =
  let ic = open_in "/Users/jakab/adventofcode/day 1/calibration_doc.txt" in
  let lines = read_file ic in
  parse_document lines
