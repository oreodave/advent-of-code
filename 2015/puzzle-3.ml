let read_all filename =
  let ic = open_in filename in
  let read_char () =
    try Some (input_char ic)
    with End_of_file -> None in
  let rec read_chars cur =
    match read_char () with
    | Some s -> read_chars (s :: cur)
    | None ->
      close_in ic; List.rev cur in
  read_chars []

let table_inc table dir =
  if Hashtbl.mem table dir then
    let new_rec = (Hashtbl.find table dir) + 1 in
    Hashtbl.replace table dir new_rec
  else
    Hashtbl.add table dir 1

let move_direction table dir direction =
  let (x, y) = dir in
  let new_dir =
    match direction with
    | '>' -> (x + 1, y)
    | '<' -> (x - 1, y)
    | '^' -> (x, y + 1)
    | 'v' -> (x, y - 1)
    | _ -> raise (Invalid_argument "Expected one of ><^v")
  in
  table_inc table new_dir;
  new_dir

let rec move_all table current directions =
  match directions with
  | [] -> ()
  | x::xs ->
    let new_dir = move_direction table current x in
    move_all table new_dir xs

let table_merge t1 t2 =
  (* Stolen from https://stackoverflow.com/a/78427785/26861165 *)
  t2
  |> Hashtbl.to_seq
  |> Hashtbl.replace_seq t1;
  ()

let () =
  let characters = read_all "3-input" in
  let table = Hashtbl.create 2081 in
  table_inc table (0, 0);
  move_all table (0, 0) characters;
  Printf.printf "Round 1: %d\n" (Hashtbl.length table);

  let santa = Hashtbl.create 1024 in
  let robosanta = Hashtbl.create 1024 in
  table_inc santa (0, 0);
  table_inc robosanta (0, 0);

  let santa_filter i _ = i mod 2 == 0 in
  let robosanta_filter i _ = i mod 2 == 1 in
  let santa_dirs = List.filteri santa_filter characters in
  let robosanta_dirs = List.filteri robosanta_filter characters in

  move_all santa (0, 0) santa_dirs;
  move_all robosanta (0, 0) robosanta_dirs;
  table_merge santa robosanta;
  Printf.printf "Round 2: %d\n" (Hashtbl.length santa);

(* Local Variables: *)
(* compile-command: "ocaml puzzle-3.ml" *)
(* End: *)
