open OUnit2
open Source
open Players
open Bank
open Cards

let test_player = add_player "test player" [] 0 0 0 false

let test_player_add = add_balance test_player 100

let test_player_payraise = payraise test_player

let test_player_debt = add_balance test_player (-25000)

let test_player_college = pay_college test_player

let test_player_children = add_children test_player 2

let test_player_so = add_significant_other test_player

let test_player_add_card = add_card police_officer test_player

let test_player_remove_card =
  remove_card police_officer test_player_add_card

let test_player_exchange_card =
  exchange_card test_player_add_card veterinarian police_officer

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
         ( "Player operation add_children" >:: fun _ ->
           assert_equal 2 test_player_children.children );
         ( "Player operation add_significant_other" >:: fun _ ->
           assert_equal true test_player_so.so );
         ( "Player operation add_card police_officer" >:: fun _ ->
           assert_equal [ police_officer ] test_player_add_card.deck );
         ( "Player operation remove_card police_officer" >:: fun _ ->
           assert_equal [] test_player_remove_card.deck );
         ( "Player operation exchange_card veterinarian for \
            police_officer"
         >:: fun _ ->
           assert_equal [ veterinarian ] test_player_exchange_card.deck
         );
         (* Havent tested bank operations payday, tax, final_balance *)
       ]

let _ = run_test_tt_main tests
