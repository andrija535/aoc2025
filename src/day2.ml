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

let invalid_ranges_part1 lower_bound upper_bound = 
  Seq.(map double_digits @@ ints 1
  |> drop_while ((>) lower_bound)
  |> take_while ((>=) upper_bound))

let invalid_ranges_part2 lower_bound upper_bound =
  let digit_count n = int_of_float (floor (log10 (float_of_int n))) + 1 in
  let digits_count_half dc x = (digit_count x) * 2 <= dc in
  let expansions n = 
    let max_digit_count = digit_count upper_bound in
    Seq.unfold (fun acc -> 
    if String.length acc > max_digit_count 
    then None 
    else 
      let acc = acc ^ acc in 
      Some (int_of_string acc,acc)) (string_of_int n)
  in
  Seq.(ints 1
    |> take_while (digits_count_half (digit_count upper_bound))
    |> map expansions
    |> flat_map Fun.id
    |> drop_while ((>) lower_bound)
    |> take_while ((>=) upper_bound)
    |> group (=)
    |> map (fun x -> Seq.uncons x |> Option.get |> fst))

let part1 (input: string): int =
  parse input
  |> List.fold_left (fun acc (low,high) ->
    let invalid_ranges = invalid_ranges_part1 low high in
    Seq.fold_left (+) acc invalid_ranges) 0

(* Still work in progress *)
let part2 (_input: string): int =
  parse _input
  |> List.fold_left (fun acc (low,high) ->
    let invalid_digits = invalid_ranges_part2 low high in
    Seq.fold_left (+) acc invalid_digits) 0

let () =
  let ic = open_in "input/day2.txt" in
  (* Input is just a single line *)
  let input = input_line ic in
  close_in ic;
  let result1 = part1 input in
  let result2 = part2 input in
  Printf.printf "Part 1: %d\nPart 2: %d\n" result1 result2
