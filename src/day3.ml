type battery_bank = int list

let parse (lines: string list): battery_bank list =
  let char_to_int c = Char.code c - Char.code '0' in
  let parse_bank s = 
    String.to_seq s
    |> Seq.map char_to_int
    |> List.of_seq
  in
  List.map parse_bank lines

module Day1 = struct
  let max_digit_parition (xs: int list): int * int list =
    let rec go (max,rest) = function
      | [] -> (max, rest)
      | [_] -> (max, rest)
      | x::xs ->
        if x > max then
          go (x,xs) xs
        else
          go (max,rest) xs
    in
    go (0,[]) xs

  let max_joltage (bank: battery_bank): int =
    let max_el = List.fold_left (fun x y -> if x > y then x else y) 0 in
    let x,rest = max_digit_parition bank in
    let y = max_el rest in
    x * 10 + y

  let run (lines: string list): int =
    let banks = parse lines in
    List.map max_joltage banks
    |> List.fold_left (+) 0
end

module Day2 = struct
  (* If you are getting e.g. the second digit, and your line length is 12, you can only
     really consider up to the 2nd character because you'll need 10 more digits to the right.
     iteration assumed to be 1-indexed *)
  let max_digit_parition (xs: int list) (iteration: int) (line_length: int): int * int list =
    let max_iterations = line_length - (12 - iteration) in
    let rec go (max,rest,i) = function
      | [] -> (max, rest)
      | [_] -> (max, rest)
      (* We need to limit iterations because we need to stop early enough *)
      | _::_ when i >= max_iterations -> (max, rest)
      | x::xs ->
        if x > max then
          go (x,xs,i+1) xs
        else
          go (max,rest,i+1) xs
    in
    go (0,[],0) xs

  let digits_to_int (digits: int Seq.t): int =
    Seq.(zip @@ ints 0) digits
    |> Seq.map (fun (i, digit) -> digit * int_of_float (10. ** float_of_int i))
    |> Seq.fold_left (+) 0

  let max_digit = List.fold_left (fun x y -> if x > y then x else y) 0

  let max_joltage (bank: battery_bank): int = 
    let first_11, rest =
      let rec go acc i xs =
        if i > 11 then acc,xs
        else
          let line_length = List.length xs in
          let max, rest' = max_digit_parition xs i line_length in
          go (max::acc) (i+1) rest'
      in
      go [] 1 bank
    in
    let last_digit = max_digit rest in
    List.to_seq first_11
    |> Seq.append (Seq.singleton last_digit)
    |> digits_to_int
    
  let run (lines: string list): int =
    let banks = parse lines in
    List.map max_joltage banks
    |> List.fold_left (+) 0
end

let () =
  let input = Helpers.read_lines "input/day3.txt" in
  let result1 = Day1.run input in
  let result2 = Day2.run input in
  Printf.printf "Part 1: %d\nPart 2: %d\n" result1 result2
