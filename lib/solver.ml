let read_file filename =
  let input_channel = open_in filename in
  let read () = try Some (input_line input_channel) with End_of_file -> None in

  let rec loop acc = match read () with
    | Some s -> loop (s :: acc)
    | None ->
      close_in input_channel;
      acc
  in
  loop []

let parse_color_count str =
  match String.split_on_char ' ' (String.trim str) with
  | [count; color] -> (int_of_string count, color)
  | _ -> raise (Invalid_argument ("Invalid color count: " ^ str))

let parse_set str = match str with
  | "" -> []
  | _ -> List.map parse_color_count (String.split_on_char ',' str)
(* ex: [(3, "blue"); (4, "red")] *)

let is_valid set =
  let check_color (count, color) =
    match color with
    | "red" -> count <= 12
    | "green" -> count <= 13
    | "blue" -> count <= 14
    | _ -> raise (Invalid_argument ("Invalid color: " ^ color))
  in
  List.for_all check_color set

let parse_line line =
  match String.split_on_char ':' line with
  | [game_id_str; sets_str] ->
      let game_id = Scanf.sscanf game_id_str "Game %d" (fun x -> x) in
      let sets =
        String.split_on_char ';' sets_str |> List.map parse_set
      in
      (game_id, List.for_all is_valid sets)
  | _ -> raise (Invalid_argument ("Invalid line: " ^ line))

let solve input =
  input
  |> List.map parse_line
  |> List.filter_map (fun (game_id, is_possible) -> if is_possible then Some game_id else None)
  |> List.fold_left (+) 0

let solve2 input =
  let parsed = List.map parse_line input in
  let valid_games = List.filter_map (fun (game_id, is_possible) -> if is_possible then Some game_id else None) parsed in
  let rec res (lst, sum) = match lst with
    | [] -> sum
    | id :: tail -> res (tail, sum + id)
  in
  res (valid_games, 0)
