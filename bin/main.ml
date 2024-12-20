open Solver

let () =
  let result = solve (read_file "input.txt") in
  Printf.printf "Result: %d\n" result
