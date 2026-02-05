let parse_file input =
  let pair = function
    | [ a; b ] -> (int_of_string a, int_of_string b)
    | _ -> failwith ""
  in
  input |> String.trim |> String.split_on_char ','
  |> List.map (fun s -> String.split_on_char '-' s |> pair)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let get_number len n = n mod pow 10 len

let rec count_digits n =
  let rec l acc n = if n > 0 then l (acc + 1) (n / 10) else acc in
  l 0 n

let collect_numbers split n =
  let len = count_digits n in
  let rec collect acc i n =
    if n > 0 then collect (get_number split n :: acc) (i + 1) (n / pow 10 split)
    else acc
  in

  if len mod split = 0 then Some (collect [] 1 n) else None

let check_repeat predicate arr =
  if predicate (List.length arr) then
    let num = List.nth arr 0 in
    List.for_all (fun n -> n = num) arr
  else false

let check split predicate n =
  match collect_numbers split n with
  | Some l -> check_repeat predicate l
  | None -> false

let rec fold_range acc n max f =
  if n > max then acc
  else
    let acc = f acc n in
    fold_range acc (n + 1) max f

let step1 acc (lower, upper) =
  let folded =
    fold_range 0 lower upper (fun accumulator n ->
        let len = count_digits n in
        let split = len / 2 in

        if split = 0 then accumulator
        else if check split (fun len -> len = 2) n then accumulator + n
        else accumulator)
  in
  acc + folded

let step2 acc (lower, upper) =
  let min_repeat = 2 in

  let rec check_split_range current_split min n =
    if current_split < min then false
    else if check current_split (fun n -> n >= min_repeat) n then true
    else check_split_range (current_split - 1) min n
  in

  let folded =
    fold_range 0 lower upper (fun acc n ->
        let len = count_digits n in
        let min_split = 1 in
        let max_split = len / min_repeat in
        if check_split_range max_split min_split n then acc + n else acc)
  in
  acc + folded

let () =
  let pairs =
    In_channel.with_open_text "2/input.txt" In_channel.input_all |> parse_file
  in

  let s1 = List.fold_left step1 0 pairs in
  let s2 = List.fold_left step2 0 pairs in

  Printf.printf "step1: %d\nstep2: %d\n" s1 s2
;;

assert (check_repeat (fun len -> len >= 3) [ 12; 12; 12 ] = true);
assert (check_repeat (fun len -> len >= 3) [ 12; 52; 82 ] = false);
assert (check_repeat (fun len -> len = 3) [ 12; 12 ] = false);
assert (check_repeat (fun len -> len = 2) [ 12; 12 ] = true);
assert (check 3 (fun len -> len = 3) 121212 = false);
assert (check 2 (fun len -> len = 3) 121212 = true);
assert (check 2 (fun len -> len = 3) 121212 = true);
assert (check 1 (fun len -> len = 1) 1 = true);
assert (check 1 (fun len -> len = 2) 11 = true);
assert (collect_numbers 3 123123123 = Some [ 123; 123; 123 ]);
assert (check 1 (fun len -> len > 1) 11 = true);
assert (step1 0 (11, 22) = 33);
assert (step1 0 (243, 401) = 0);
assert (step2 0 (11, 22) = 33);
assert (step2 0 (111, 222) = 333);
assert (step2 0 (243, 401) = 333)
