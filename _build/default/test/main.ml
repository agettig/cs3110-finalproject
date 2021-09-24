open OUnit2
open Source

let player1 : Types.player = {name = "Connor"; children = 0; so = false;
deck = []; account_balance = 0; debt = 0; pay_raise = 0}

let player2 = Bank.add_balance player1 100

let tests = "test suite for sum" >::: [
  "Type player intialization" >:: (fun _ -> assert_equal 0 player1.account_balance);
  "Bank operation add_balance" >:: (fun _ -> assert_equal 100 player2.account_balance);
]

let _ = run_test_tt_main tests