open OUnit2
open Source
open Players
open Bank
open Cards
open Gamestate
open Tiles

let test_player = add_player "test player" false

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

let test_player_payday = payday test_player_add_card

let test_player_tax = tax test_player_payday

let test_player_tax_debt = tax test_player_add_card

let test_player_payday_cap =
  payday
    (test_player_add_card |> payraise |> payraise |> payraise
   |> payraise |> payraise)

let test_player_payday_raise_no_cap =
  payday (test_player_add_card |> payraise)

let test_player_final_balance =
  final_balance
    (test_player_tax_debt |> add_card mobile_home
   |> add_card dbl_wide_rv
    |> add_card (Life_Tiles 10000)
    |> add_card (Life_Tiles 40000))

let test_player_final_balance1 =
  test_player_tax_debt |> add_card mobile_home |> add_card dbl_wide_rv
  |> add_card (Life_Tiles 10000)
  |> add_card (Life_Tiles 40000)

let test_player_final_balance2 =
  test_player_tax_debt |> add_card mobile_home |> add_card dbl_wide_rv
  |> add_card (Life_Tiles 10000)
  |> add_card (Life_Tiles 50000)

let test_player_final_balance3 =
  test_player_tax_debt |> add_card mobile_home |> add_card dbl_wide_rv
  |> add_card (Life_Tiles 40000)
  |> add_card (Life_Tiles 40000)

(* Need to test functions in gamestate - change_index_board -
   player_turn *)

(* Need to test function in player - add_player *)

let test_tile =
  PayTile
    {
      name = "START COLLEGE!";
      account_change = -100000;
      index_tile = 1;
    }

let bad_test_tile =
  PayTile
    { name = "Initial State!"; account_change = 0; index_tile = 0 }

let test_player_index_change = change_index_board test_player

let test_player_stop_1 =
  { test_player with index_on_board = 9; college = true }

let test_player_stop_2 =
  { test_player with index_on_board = 24; college = true }

let test_player_stop_3 =
  { test_player with index_on_board = 32; college = true }

let test_player_stop_4 =
  { test_player with index_on_board = 96; college = true }

let test_player_stop_5 =
  { test_player with index_on_board = 129; college = true }

let test_player_finished =
  { test_player with index_on_board = final_tile_index }

let test_gamestate =
  {
    current_player = test_player;
    players = [ test_player; test_player_add ];
    tiles = [];
    deck = [];
  }

let test_gamestate_finished =
  {
    current_player = test_player_finished;
    players = [ test_player_finished ];
    tiles = [];
    deck = [];
  }

(* let print_terminal = test_player_final_balance3 |>
   player_to_string *)

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
           assert_equal 15000 test_player_debt.account_balance );
         ( "Bank operation calculate_loans balance" >:: fun _ ->
           assert_equal 15000
             (Bank.calculate_loans test_player_debt).account_balance );
         ( "Bank operation calculate_loans debt" >:: fun _ ->
           assert_equal 50000
             (Bank.calculate_loans test_player_debt).debt );
         ( "Bank operation pay_college" >:: fun _ ->
           assert_equal 100000 test_player_college.debt );
         ( "Bank operation payday no payraise" >:: fun _ ->
           assert_equal 40000 test_player_payday.account_balance );
         ( "Bank operation payday payraise" >:: fun _ ->
           assert_equal 50000
             test_player_payday_raise_no_cap.account_balance );
         ( "Bank operation payday salary max cap" >:: fun _ ->
           assert_equal 70000 test_player_payday_cap.account_balance );
         ( "Bank operation tax no debt" >:: fun _ ->
           assert_equal 25000 test_player_tax.account_balance );
         ( "Bank operation tax debt check balance" >:: fun _ ->
           assert_equal 5000 test_player_tax_debt.account_balance );
         ( "Bank operation tax debt check debt" >:: fun _ ->
           assert_equal 25000 test_player_tax_debt.debt );
         ( "Bank operation final balance" >:: fun _ ->
           assert_equal 410000 test_player_final_balance );
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
         ( "Gamestate operation change_index_board" >:: fun _ ->
           assert_equal false
             (test_player_index_change.index_on_board
            = test_player.index_on_board) );
         ( "Gamestate operation change_index_board stop college"
         >:: fun _ ->
           assert_equal 10
             (change_index_board test_player_stop_1).index_on_board );
         ( "Gamestate operation change_index_board stop marriage"
         >:: fun _ ->
           assert_equal 25
             (change_index_board test_player_stop_2).index_on_board );
         ( "Gamestate operation change_index_board stop starter home"
         >:: fun _ ->
           assert_equal 33
             (change_index_board test_player_stop_3).index_on_board );
         ( "Gamestate operation change_index_board stop house"
         >:: fun _ ->
           assert_equal 97
             (change_index_board test_player_stop_4).index_on_board );
         ( "Gamestate operation change_index_board stop retire"
         >:: fun _ ->
           assert_equal 130
             (change_index_board test_player_stop_5).index_on_board );
         ( "Gamestate operation current_player" >:: fun _ ->
           assert_equal test_player (current_player test_gamestate) );
         ( "Gamestate operation finished false" >:: fun _ ->
           assert_equal false (finished test_player) );
         ( "Gamestate operation finished true" >:: fun _ ->
           assert_equal true (finished test_player_finished) );
         ( "Gamestate operation next_player" >:: fun _ ->
           assert_equal test_player_add
             (next_player test_player test_gamestate.players) );
         ( "Gamestate operation next_player wrap" >:: fun _ ->
           assert_equal test_player
             (next_player test_player_add test_gamestate.players) );
         ( "Gamestate operation gameover false" >:: fun _ ->
           assert_equal false (gameover test_gamestate.players) );
         ( "Gamestate operation gameover true" >:: fun _ ->
           assert_equal true (gameover test_gamestate_finished.players)
         );
         ( "Gamestate operation get_tile true" >:: fun _ ->
           assert_equal true (get_tile 1 gold_tiles = test_tile) );
         ( "Gamestate operation get_tile false" >:: fun _ ->
           assert_equal false (get_tile 1 gold_tiles = bad_test_tile) );
         ( "Gamestate operation player_winner" >:: fun _ ->
           assert_equal test_player_final_balance3
             (player_winner
                [
                  test_player_final_balance1;
                  test_player_final_balance2;
                  test_player_final_balance3;
                ]
                test_player_final_balance1) );
       ]

let _ = run_test_tt_main tests
