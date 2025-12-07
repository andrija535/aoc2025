type range = int * int

let parse (input: string): range list =
  String.split_on_char ',' input
  |> List.map (fun part ->
    match String.split_on_char '-' part with
    | [a; b] -> (int_of_string a, int_of_string b)
    | _ -> failwith "Invalid range format")

module Day1 = struct
  let double_digits x =
    let s = string_of_int x in
    int_of_string (s ^ s)

  let invalid_ranges lower_bound upper_bound = 
    Seq.(map double_digits @@ ints 1
    |> drop_while ((>) lower_bound)
    |> take_while ((>=) upper_bound))

  let run (input: string): int =
    parse input
    |> List.fold_left (fun acc (low,high) ->
      let invalid_ranges = invalid_ranges low high in
      Seq.fold_left (+) acc invalid_ranges) 0
end

module Day2 = struct
  let digit_count n = int_of_float (floor (log10 (float_of_int n))) + 1

  let repeating_progression (max_length: int) (n: int): int Seq.t =
    let n_len = digit_count n in
    let max_repeats = max_length / n_len in
    let n_str = string_of_int n in
    let res = 
      Seq.init max_repeats (fun i -> 
        int_of_string @@
        String.concat "" (List.init (i+1) @@ Fun.const n_str))
    in
    if n_len = 1 then Seq.drop 1 res else res

  let seq_distinct = Fun.compose (Seq.flat_map (Seq.take 1)) (Seq.group (=))

  let invalid_ranges lower_bound upper_bound =
    let max_can_repeat = int_of_string @@ String.init (digit_count upper_bound / 2) (Fun.const '9') in
    let max_digits = digit_count upper_bound in
    Seq.(init max_can_repeat (fun i -> i + 1)
    |> flat_map (repeating_progression max_digits)
    |> filter (fun x -> x >= lower_bound && x <= upper_bound))
    |> seq_distinct

  let run (input: string): int =
    parse input
    |> List.fold_left (fun acc (low,high) ->
      let invalid_ranges = invalid_ranges low high in
      (* Debug output *)
      (* Printf.printf "Invalid ranges between %d and %d: " low high; *)
      (* Seq.iter (Printf.printf "%d ") invalid_ranges; *)
      (* print_newline (); *)
      Seq.fold_left (+) acc invalid_ranges) 0
end

let () =
  let ic = open_in "input/day2.txt" in
  (* Input is just a single line *)
  let input = input_line ic in
  close_in ic;
  let result1 = Day1.run input in
  let result2 = Day2.run input in
  Printf.printf "Part 1: %d\nPart 2: %d\n" result1 result2
