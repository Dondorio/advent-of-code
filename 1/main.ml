let modulo x y =
  let r = x mod y in
  if r >= 0 then r else r + y

let f (rotation, result) data =
  let distance = String.sub data 1 (String.length data - 1) |> int_of_string in
  let m = if String.get data 0 = 'L' then -1 else 1 in
  let r = modulo (rotation + (distance * m)) 100 in
  (r, if r = 0 then result + 1 else result)

let _, res =
  In_channel.with_open_text "1/input.txt" In_channel.input_lines
  |> List.fold_left f (50, 0)
;;

print_int res
