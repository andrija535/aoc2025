type range = int * int

let parse (input: string): range list =
  String.split_on_char ',' input
  |> List.map (fun part ->
    match String.split_on_char '-' part with
    | [a; b] -> (int_of_string a, int_of_string b)
    | _ -> failwith "Invalid range format")

let double_digits x =
  let s = string_of_int x in
  int_of_string (s ^ s)

let invalid_ranges lower_bound upper_bound = 
  Seq.(map double_digits @@ ints 1
  |> drop_while ((>) lower_bound)
  |> take_while ((>=) upper_bound))

let part1 (input: string): int =
  parse input
  |> List.fold_left (fun acc (low,high) ->
    let invalid_ranges = invalid_ranges low high in
    Seq.fold_left (+) acc invalid_ranges) 0


let part2 (_input: string): int = 0 

let () =
  let ic = open_in "input/day2.txt" in
  (* Input is just a single line *)
  let input = input_line ic in
  close_in ic;
  let result1 = part1 input in
  let result2 = part2 input in
  Printf.printf "Part 1: %d\nPart 2: %d\n" result1 result2
