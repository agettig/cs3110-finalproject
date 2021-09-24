(* test test *)
open Source

let () = print_endline "Hello, world! - main file"

let () = print_endline (string_of_int Board.spinner)

let player1 : Types.player = {name = "Connor"; children = 0; so = false; deck = []; account_balance = 0; debt = 0; pay_raise = 0}

let () = print_endline (string_of_int player1.account_balance)

let add_money (player : Types.player) (amount : int) : Types.player = 
  {player with account_balance = player.account_balance + amount}
let player1 = add_money player1 100

let () = print_endline (string_of_int player1.account_balance)

