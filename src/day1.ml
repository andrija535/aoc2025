let parse (s: string): int =
  let p,n = s.[0], String.sub s 1 (String.length s - 1) in
  let n = int_of_string n in
  match p with
  | 'L' -> -n
  | 'R' -> n
  | _ -> failwith "Invalid direction"

let (%) a b =
  let r = a mod b in
  if r < 0 then r + b else r

let part1 (lines: string list) : int =
  lines
  |> List.map parse
  |> List.fold_left (fun (pos,t) x ->
      let pos = (pos + x) % 100 in
      let t = if pos = 0 then t + 1 else t in
      pos,t) (50,0)
  |> Pair.snd

let part2 (lines: string list) : int =
  lines
  |> List.map parse
  |> List.fold_left (fun (pos,t) x ->
      let pos = if pos = 0 && x < 0 then 100 else pos in
      let turns = if x < 0 then (x - 100 + pos) / (-100) else (x+pos) / 100 in
      let pos = (pos + x) % 100 in
      (pos, t + turns)) (50,0)
  |> Pair.snd

let () =
  let input = Helpers.read_lines "input/day1.txt" in
  let result1 = part1 input in
  let result2 = part2 input in
  Printf.printf "Part 1: %d\nPart 2: %d\n" result1 result2
