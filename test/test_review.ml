open OUnit2
open Solver

let suite =
  "Solver Tests" >::: [
    (* Test parse_color_count with valid input *)
    "parse_color_count valid input" >:: (fun _ ->
      assert_equal (parse_color_count "3 blue") (3, "blue")
    );

    (* Test parse_color_count with invalid input *)
    "parse_color_count invalid input" >:: (fun _ ->
      assert_raises (Invalid_argument "Invalid color count: invalid")
        (fun () -> parse_color_count "invalid")
    );

    (* Test parse_set with valid input *)
    "parse_set valid input" >:: (fun _ ->
      assert_equal (parse_set "3 blue, 4 red") [(3, "blue"); (4, "red")]
    );

    (* Test parse_set with empty input *)
    "parse_set empty input" >:: (fun _ ->
      assert_equal (parse_set "") []
    );

    (* Test is_valid with valid set *)
    "is_valid valid set" >:: (fun _ ->
      assert_equal (is_valid [(3, "blue"); (4, "red")]) true
    );

    (* Test is_valid with invalid set (exceeding count) *)
    "is_valid invalid set count" >:: (fun _ ->
      assert_equal (is_valid [(15, "blue")]) false
    );

    (* Test is_valid with invalid set (unknown color) *)
    "is_valid invalid set color" >:: (fun _ ->
      assert_raises (Invalid_argument "Invalid color: yellow")
        (fun () -> is_valid [(3, "yellow")])
    );

    (* Test parse_line with valid input *)
    "parse_line valid input" >:: (fun _ ->
      assert_equal (parse_line "Game 1: 3 blue,4 red;5 green")
        (1, true)
    );

    (* Test parse_line with invalid input format *)
    "parse_line invalid input" >:: (fun _ ->
      assert_raises (Invalid_argument "Invalid line: invalid")
        (fun () -> parse_line "invalid")
    );

    "parse_line invalid set" >:: (fun _ ->
      assert_equal (parse_line "Game 2: 15 blue,4 red") (2, false)
    );

    "solve mixed games" >:: (fun _ ->
      let input = [
        "Game 1: 3 blue,4 red;5 green";
        "Game 2: 15 blue,4 red";
        "Game 3: 3 blue,12 red"
      ] in
      assert_equal (solve input) 4  (* 1 + 3 = 4 *)
    );

    "solve no valid games" >:: (fun _ ->
      let input = ["Game 1: 15 blue,20 red"] in
      assert_equal (solve input) 0
    );

    "solve all valid games" >:: (fun _ ->
      let input = [
        "Game 1: 3 blue,4 red";
        "Game 2: 5 green"
      ] in
      assert_equal (solve input) 3  (* 1 + 2 = 3 *)
    );
  ]

let () =
  run_test_tt_main suite
