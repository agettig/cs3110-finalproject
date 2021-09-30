open OUnit2
open Source
open Players
open Bank

let test_player : player =
  {
    name = "test player";
    children = 0;
    so = false;
    deck = [];
    account_balance = 0;
    debt = 0;
    pay_raise = 0;
    college = false;
    index_on_board = 0;
  }

let test_player_add = add_balance test_player 100

let test_player_payraise = payraise test_player

let test_player_debt = add_balance test_player (-25000)

let test_player_college = pay_college test_player

let tests =
  "test suite for sum"
  >::: [
         ( "Type player intialization" >:: fun _ ->
           assert_equal 0 test_player.account_balance );
         ( "Bank operation add_balance" >:: fun _ ->
           assert_equal 100 test_player_add.account_balance );
         ( "Bank operation payraise" >:: fun _ ->
           assert_equal 10000 test_player_payraise.pay_raise );
         ( "Bank operation add_balance negative" >:: fun _ ->
           assert_equal (-25000) test_player_debt.account_balance );
         ( "Bank operation calculate_loans balance" >:: fun _ ->
           assert_equal 15000
             (Bank.calculate_loans test_player_debt).account_balance );
         ( "Bank operation calculate_loans debt" >:: fun _ ->
           assert_equal 50000
             (Bank.calculate_loans test_player_debt).debt );
         ( "Bank operation pay_college" >:: fun _ ->
           assert_equal 100000 test_player_college.debt );
         (* Havent tested bank operations payday, tax, final_balance *)
         (* Havent tested card operations add_card, remove_card, exchange_cards *)
       ]

let _ = run_test_tt_main tests
