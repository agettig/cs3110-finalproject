(* test test *)
open Source

let () = print_endline "Hello, world! - main file"

let () = print_endline (string_of_int Board.spinner)

let player1 : Types.player = {name = "Connor"; children = 0; so = false; deck = []; account_balance = 0; debt = 0; pay_raise = 0}