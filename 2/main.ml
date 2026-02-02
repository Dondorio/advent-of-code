let pair = function [ a; b ] -> (a, b) | _ -> failwith ""

let parse_file input =
  input |> String.trim |> String.split_on_char ','
  |> List.map (fun s -> String.split_on_char '-' s |> pair)

let check n =
  let str = string_of_int n in
  let len = String.length str in
  match len mod 2 = 0 with
  | true ->
      let half = len / 2 in
      let l = String.sub str 0 half in
      let r = String.sub str half half in
      l = r
  | false -> false

let f acc (lower, upper) =
  let lower = int_of_string lower in
  let upper = int_of_string upper in

  let rec l acc n max =
    match n > max with
    | true -> acc
    | false ->
        let acc = if check n then acc + n else acc in
        l acc (n + 1) max
  in

  acc + l 0 lower upper
;;

In_channel.with_open_text "2/input.txt" In_channel.input_all
|> parse_file |> List.fold_left f 0 |> print_int;

print_endline ""
