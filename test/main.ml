open OUnit2
open Source
open Players
open Bank
open Cards
open Gamestate
open Tiles
open Board

(* TEST PLAN: A large majority of the functions seen in the .mli files,
   such as gamestate.mli, players.mli and bank.mli, are automatically
   tested using OUnit. However, many functions' outputs depend on user
   inputted data, making them difficult to test using OUnit. As a
   result, these functions, such as gamestate.turn, were tested manually
   through the use of 'make play'. After running 'make play,' we tested
   many variations of the game to ensure that our code was working as
   intended. Functions from gamestate, players, and bank were tested
   using OUnit with black box testing methods, in which we inputted data
   and tested based on its specification. This testing approach was the
   best possible way to demonstrate the correctness of the system
   because the OUnit tests show the functions that assist in running the
   game. Then, by manually testing the actual game extensively through
   'make play,' we are able to catch common errors that a user may
   experience. *)

let police_officer =
  Career
    {
      name = "Police Officer";
      salary = 40000;
      salary_max = 70000;
      taxes_due = 15000;
      college_career = false;
    }

let veterinarian =
  Career
    {
      name = "Veterinarian";
      salary = 100000;
      salary_max = 120000;
      taxes_due = 35000;
      college_career = true;
    }

let mobile_home =
  House
    {
      name = "Mobile Home";
      price = 80000;
      selling_price = 80000;
      starter = true;
    }

let dbl_wide_rv =
  House
    {
      name = "Double Wide + RV";
      price = 300000;
      selling_price = 300000;
      starter = false;
    }

let deck_spin_exemption = [ SpinToWin_Card 2; Exemption_Card ]

let deck_life_tiles_house = [ Life_Tiles 1; dbl_wide_rv ]

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
    graphics = false;
  }

let test_gamestate_new_deck =
  {
    current_player = test_player;
    players = [ test_player ];
    tiles = [];
    deck = deck_life_tiles_house;
    graphics = false;
  }

let test_gamestate_no_stops =
  {
    current_player = test_player_stop_3;
    players = [ test_player_stop_3; test_player_stop_4 ];
    tiles = [];
    deck = [];
    graphics = false;
  }

let test_gamestate_finished =
  {
    current_player = test_player_finished;
    players = [ test_player_finished ];
    tiles = [];
    deck = [];
    graphics = false;
  }

let test_player2 = add_player "player 2" false

(* player on starter home tile *)
let starter_home_player =
  { test_player with name = "starter home"; index_on_board = 33 }

(* player on second house tile *)
let home_player =
  { test_player with name = "home"; index_on_board = 97 }

(* player on the career tile*)
let career_player =
  { test_player with name = "career"; index_on_board = 11 }

(* player on the married tile*)
let married_player =
  { test_player with name = "married"; index_on_board = 25 }

(* marriage tile *)
let married_tile =
  FamilyTile
    {
      name = "Get Married";
      account_change = 0;
      index_tile = 25;
      children = 0;
    }

(* elope_player *)
let elope_player =
  { test_player with name = "elope"; index_on_board = 20 }

(*elope player to marriage*)
let elope_married_player =
  { test_player with name = "elope"; index_on_board = 25; so = true }

(* elope tile *)
let elope_tile =
  FamilyTile
    {
      name = "Elope!";
      account_change = 0;
      index_tile = 20;
      children = 0;
    }

(* children tile*)
let baby_tile =
  FamilyTile
    {
      name = "Baby Boy!";
      account_change = 0;
      index_tile = 37;
      children = 1;
    }

(* player with 1 child *)
let family_player = { test_player with children = 1 }

(* player who won scholarship of $20000 *)
let scholarship_player = { test_player with account_balance = 30000 }

let paytile =
  PayTile
    { name = "Scholarship"; account_change = 20000; index_tile = 2 }

let tax_tile = TaxesTile { name = "Taxes"; index_tile = 61 }

let vet =
  {
    name = "vet";
    children = 0;
    deck = [ veterinarian ];
    so = false;
    college = true;
    debt = 0;
    pay_raise = 0;
    account_balance = 100000;
    index_on_board = 40;
  }

let taxes_vet = { vet with account_balance = 65000 }

(* let print_terminal = test_player_final_balance3 |>
   player_to_string *)

let test_int (name : string) (exp_out : int) (act_out : int) =
  name >:: fun _ -> assert_equal exp_out act_out ~printer:string_of_int

(* let test_str (name : string) (exp_out : string) (act_out : string) =
   name >:: fun _ -> assert_equal exp_out act_out
   ~printer:String.escaped *)

let test_bool (name : string) (exp_out : bool) (act_out : bool) =
  name >:: fun _ -> assert_equal exp_out act_out ~printer:string_of_bool

let test_list (name : string) (exp_out : 'a list) (act_out : 'a list) =
  name >:: fun _ -> assert_equal exp_out act_out

let test_pl (name : string) (exp_out : player) (act_out : player) =
  name >:: fun _ -> assert_equal exp_out act_out

let test_int (name : string) (exp_out : int) (act_out : int) =
  name >:: fun _ -> assert_equal exp_out act_out

let test_string (name : string) (exp_out : string) (act_out : string) =
  name >:: fun _ -> assert_equal exp_out act_out ~printer:(fun x -> x)

let test_cards_option
    (name : string)
    (exp_out : cards option)
    (act_out : cards option) =
  name >:: fun _ -> assert_equal exp_out act_out

let test_next_player_stop_tiles
    (name : string)
    (expected_and_current : player)
    (player_lst : player list) =
  name >:: fun _ ->
  assert_equal expected_and_current
    (next_player expected_and_current player_lst) ~printer:(fun x ->
      x.name)

let test_new_deck_helper
    (name : string)
    (expected : cards list)
    (card_opt : cards option * cards option)
    (gamestate : gamestate) =
  name >:: fun _ ->
  assert_equal expected (new_deck_helper card_opt gamestate)

let test_assert_raises (name : string) exp_out act_out =
  name >:: fun _ -> assert_raises exp_out act_out

let play_str player = player.name ^ string_of_int player.account_balance

let rec print_players = function
  | [] -> ""
  | h :: t -> play_str h ^ print_players t

let test_new_player_helper
    (name : string)
    expected
    player
    tile
    gamestate =
  name >:: fun _ ->
  assert_equal expected (new_player_helper tile player gamestate)
    ~printer:(fun x -> print_players (fst x))

let tests =
  "test suite for sum"
  >::: [
         test_int "Type player intialization" 10000
           test_player.account_balance;
         test_int "Bank operation add_balance" 10100
           test_player_add.account_balance;
         test_int "Bank operation payraise" 10000
           test_player_payraise.pay_raise;
         test_int "Bank operation add_balance negative" 5000
           test_player_debt.account_balance;
         test_int "Bank operation calculate_loans balance" 5000
           (Bank.calculate_loans test_player_debt).account_balance;
         test_int "Bank operation calculate_loans debt" 25000
           (Bank.calculate_loans test_player_debt).debt;
         test_int "Bank operation pay_college" 100000
           test_player_college.debt;
         test_int "Bank operation payday no payraise" 50000
           test_player_payday.account_balance;
         test_int "Bank operation payday payraise" 60000
           test_player_payday_raise_no_cap.account_balance;
         test_int "Bank operation payday salary max cap" 80000
           test_player_payday_cap.account_balance;
         test_int "Bank operation tax no debt" 35000
           test_player_tax.account_balance;
         test_int "Bank operation tax debt check balance" 15000
           test_player_tax_debt.account_balance;
         test_int "Bank operation tax debt check debt" 25000
           test_player_tax_debt.debt;
         test_int "Bank operation final balance" 420000
           test_player_final_balance;
         test_int "Player operation add_children" 2
           test_player_children.children;
         test_bool "Player operation add_significant_other" true
           test_player_so.so;
         test_list "Player operation add_card police_officer"
           [ police_officer ] test_player_add_card.deck;
         test_list "Player operation remove_card police_officer" []
           test_player_remove_card.deck;
         test_list
           "Player operation exchange_card veterinarian for \
            police_officer"
           [ veterinarian ] test_player_exchange_card.deck;
         test_bool "Gamestate operation change_index_board" false
           ((fst test_player_index_change).index_on_board
          = test_player.index_on_board);
         test_int "Gamestate operation change_index_board stop college"
           10
           (fst (change_index_board test_player_stop_1)).index_on_board;
         test_int "Gamestate operation change_index_board stop marriage"
           25
           (fst (change_index_board test_player_stop_2)).index_on_board;
         test_int
           "Gamestate operation change_index_board stop starter home" 33
           (fst (change_index_board test_player_stop_3)).index_on_board;
         test_int "Gamestate operation change_index_board stop house" 97
           (fst (change_index_board test_player_stop_4)).index_on_board;
         test_int "Gamestate operation change_index_board stop retire"
           130
           (fst (change_index_board test_player_stop_5)).index_on_board;
         test_bool "Gamestate operation finished false" false
           (finished test_player);
         test_bool "Gamestate operation finished true" true
           (finished test_player_finished);
         test_pl "Gamestate operation next_player" test_player_stop_4
           (next_player test_gamestate_no_stops.current_player
              test_gamestate_no_stops.players);
         test_pl "Gamestate operation next_player wrap" test_player
           (next_player test_player_add test_gamestate.players);
         test_bool "Gamestate operation gameover false" false
           (gameover test_gamestate.players);
         test_bool "Gamestate operation gameover true" true
           (gameover test_gamestate_finished.players);
         test_bool "Gamestate operation get_tile true" true
           (get_tile 1 = test_tile);
         test_bool "Gamestate operation get_tile false" false
           (get_tile 1 = bad_test_tile);
         test_pl "Gamestate operation player_winner"
           test_player_final_balance3
           (player_winner
              [
                test_player_final_balance1;
                test_player_final_balance2;
                test_player_final_balance3;
              ]
              test_player_final_balance1);
         test_string "test normalize text" "test"
           (normalize_text "        TesT    ");
         test_next_player_stop_tiles "test next_player married player"
           married_player
           [ married_player; test_player2 ];
         test_next_player_stop_tiles
           "test next_player starter home player" starter_home_player
           [ starter_home_player; test_player2 ];
         test_next_player_stop_tiles "test next_player home player"
           home_player
           [ starter_home_player; test_player2; home_player ];
         test_next_player_stop_tiles "test next_player career player"
           career_player
           [
             starter_home_player;
             test_player2;
             career_player;
             home_player;
           ];
         test_assert_raises "test next player with player not in list"
           (Failure "Not found") (fun () ->
             next_player test_player
               [ starter_home_player; test_player2 ]);
         test_cards_option "test has_spin_card - no spin card" None
           (has_spin_card deck_life_tiles_house);
         test_cards_option "test has_spin_card - spin card"
           (Some (SpinToWin_Card 2))
           (has_spin_card deck_spin_exemption);
         test_cards_option "test has_exemption_card - no exemption card"
           None
           (has_exemption_card deck_life_tiles_house);
         test_cards_option "test has_exemption_card - exemption card"
           (Some Exemption_Card)
           (has_exemption_card deck_spin_exemption);
         test_int "test number of guesses with no spin card" 1
           (num_of_guesses_helper None);
         test_int "test number of guesses with spin card 2" 2
           (num_of_guesses_helper (Some (SpinToWin_Card 2)));
         test_assert_raises "test number of guesses with career card"
           (Failure "Illegal") (fun () ->
             num_of_guesses_helper (Some veterinarian));
         test_new_deck_helper
           "card options (None, None) returns same deck"
           deck_life_tiles_house (None, None) test_gamestate_new_deck;
         test_new_deck_helper
           "card options (Some (dbl_wide_rv), None) returns same deck"
           [ Life_Tiles 1 ]
           (Some dbl_wide_rv, None)
           test_gamestate_new_deck;
         test_new_deck_helper
           "card options (Some (dbl_wide_rv), Some Exemption Card) \
            returns same deck"
           [ Life_Tiles 1; Exemption_Card ]
           (Some dbl_wide_rv, Some Exemption_Card)
           test_gamestate_new_deck;
         test_new_deck_helper
           "card options (None, Some Exemption Card) returns same deck"
           (Exemption_Card :: deck_life_tiles_house)
           (None, Some Exemption_Card)
           test_gamestate_new_deck;
         test_list "life tile cards in list - no life tile cards" []
           (life_cards_in_deck [] deck_spin_exemption);
         test_list "life tile cards in list - 1 life tile card"
           [ Life_Tiles 1 ]
           (life_cards_in_deck [] deck_life_tiles_house);
         test_new_player_helper "test new_player_helper for action tile"
           ([ test_player ], (None, None))
           test_player
           (ActionTile
              { name = ""; position_change = 0; index_tile = 0 })
           test_gamestate;
         test_new_player_helper
           "test new_player_helper for family marriage tile"
           ([ { married_player with so = true } ], (None, None))
           married_player married_tile test_gamestate;
         test_new_player_helper
           "test new_player_helper for family elope tile"
           ([ elope_married_player ], (None, None))
           elope_player elope_tile test_gamestate;
         test_new_player_helper
           "test new_player_helper for family children tile"
           ([ family_player ], (None, None))
           test_player baby_tile test_gamestate;
         test_new_player_helper "test new_player_helper for pay tile"
           ([ scholarship_player ], (None, None))
           test_player paytile test_gamestate;
         test_new_player_helper "test new_player_helper for tax tile"
           ([ taxes_vet ], (None, None))
           vet tax_tile test_gamestate;
       ]

let _ = run_test_tt_main tests
