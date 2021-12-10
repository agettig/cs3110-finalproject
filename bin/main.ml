open Source.Gamestate
open Source.Players
open Source.Bank
open Source.Cards
open Source.Tiles
open Source.Board

open Source.Printing
(** [play_game f] starts the adventure in file [f]. *)

(** [main ()] prompts for the game to play, then starts it. *)

(** [new_player] constructs a new player with a user inputted name and
    whether or not they are going to college*)

let new_player () =
  let () = print_string "\nEnter player name: " in
  let name = read_line () in
  let print_q () =
    print_string "\nDo you want to go to college? Input yes or no\n> "
  in
  let rec college () =
    print_q ();
    match read_line () with
    | x ->
        if x |> normalize_text |> String.equal "yes" then true
        else if x |> normalize_text |> String.equal "no" then false
        else (
          print_endline "\nInvalid input";
          college ())
  in
  let print_q1 () =
    print_string "\nAre you colorblind? Input yes or no \n> "
  in

  let color_options () =
    print_string "\nAre you red-green or blue-yellow colorblind?: "
  in
  let rec choose_colorblind () =
    color_options ();
    match read_line () with
    | c ->
        if c |> normalize_text |> String.equal "red-green" then RedGreen
        else if c |> normalize_text |> String.equal "blue-yellow" then
          BlueYellow
        else (
          print_endline "\nInvalid input";
          choose_colorblind ())
  in
  let rec check_colorblind () =
    print_q1 ();
    match read_line () with
    | x ->
        if x |> normalize_text |> String.equal "yes" then
          choose_colorblind ()
        else if x |> normalize_text |> String.equal "no" then Not
        else (
          print_endline "\nInvalid input";
          check_colorblind ())
  in

  let init_player =
    add_player (String.trim name) (college ()) (check_colorblind ())
  in
  let print_q2 () =
    print_string
      "\n\
       Do you want to buy a long term investment? Input yes or no \n\
       > "
  in
  let print_q3 () = print_string "\nEnter a number 1 through 10: " in
  let rec num () =
    print_q3 ();
    match read_line () with
    | y ->
        (* need to check for numbers instead of yes *)
        let trimmed = y |> String.trim in
        if
          String.equal trimmed "1"
          || String.equal trimmed "2"
          || String.equal trimmed "3"
          || String.equal trimmed "4"
          || String.equal trimmed "5"
          || String.equal trimmed "6"
          || String.equal trimmed "7"
          || String.equal trimmed "8"
          || String.equal trimmed "9"
          || String.equal trimmed "10"
        then
          add_balance
            (add_card
               (List.nth lg_tm_invt (int_of_string trimmed - 1))
               init_player)
            (-1 * 10000)
        else (
          print_endline "\nInvalid input";
          num ())
  in
  let rec buy () =
    print_q2 ();
    match read_line () with
    | x ->
        if x |> normalize_text |> String.equal "yes" then num ()
        else if x |> normalize_text |> String.equal "no" then
          init_player
        else (
          print_endline "\nInvalid input";
          buy ())
  in
  buy ()

let rec get_all_investments (deck : cards list) (acc : int list) :
    int list =
  match deck with
  | [] -> acc
  | h :: t -> (
      match h with
      | Long_Term_Investment x -> get_all_investments t (x :: acc)
      | _ -> get_all_investments t acc)

let rec check_dup_investments (players : player list) (acc : int list) :
    bool =
  match players with
  | [] ->
      List.compare_lengths (List.sort compare acc)
        (List.sort_uniq compare acc)
      <> 0
  | h :: t ->
      check_dup_investments t (get_all_investments h.deck [] @ acc)

(** [get_players number_players acc] recursively constructs the list of
    players in the game*)
let rec get_players num_players acc =
  match num_players with
  | 0 -> acc
  | h ->
      let newbie = new_player () in
      if check_dup_investments (acc @ [ newbie ]) [] then
        let () =
          print_endline
            "\n\
             Two players cannot have identical long term investments - \
             restart creation of this player"
        in
        get_players h acc
      else get_players (h - 1) (acc @ [ newbie ])

let rec new_player_share_wealth_cards
    (current_cards : cards list)
    (num : int)
    (acc : cards list) : cards list =
  match num with
  | 0 -> acc
  | _ ->
      let new_share_card =
        let rand () = Random.int (List.length current_cards) in
        let num = rand () in
        List.nth current_cards num
      in
      new_player_share_wealth_cards
        (remove_first_instance new_share_card current_cards [])
        (num - 1) (new_share_card :: acc)

let new_share_player player share_list =
  { player with deck = player.deck @ share_list }

let rec new_share_deck player_cards current_cards =
  match player_cards with
  | [] -> current_cards
  | h :: t ->
      new_share_deck (remove_first_instance h current_cards []) t

let rec share_players
    (player_lst : player list)
    (cards_lst : cards list)
    (acc : player list) : player list * cards list =
  match player_lst with
  | [] -> (acc, cards_lst)
  | h :: t ->
      let player_cards =
        new_player_share_wealth_cards share_wealth_cards 3 []
      in
      let new_player = new_share_player h player_cards in
      let new_deck = new_share_deck player_cards share_wealth_cards in
      share_players t new_deck (acc @ [ new_player ])

let init_state tiles deck players graphics =
  {
    tiles;
    deck;
    current_player = List.nth players 0;
    players;
    graphics;
  }

let main () =
  print_iter print_life 0 11 true;
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\n\nWelcome to the Game of Life.\n";

  let ask_tutorial () =
    print_string "\nDo you want a tutorial? Input yes or no\n> "
  in
  let rec tut () =
    ask_tutorial ();
    match read_line () with
    | x ->
        if x |> normalize_text |> String.equal "yes" then
          print_endline
            "\n\
             Tutorial: GAME OF LIFE \n\
            \            \n\
             COLORBLIND OPTION:\n\
             These are the colors of each tile based off of what \
             colorblind option you inputted:\n\n\
             Not:\n\
            \     - LifeTile: yellow\n\
            \     - PayTile: green\n\
            \     - TaxesTile: yellow\n\
            \     - CareerTile: red\n\
            \     - HouseTile: red\n\
            \     - TakeTile: yellow\n\
            \     - ActionTile: yellow\n\
            \     - LawsuitTile: blue\n\
            \     - FamilyTile: yellow\n\
            \     - SpintoWinTile: yellow\n\n\
             RedGreen:\n\
            \     - LifeTile: yellow\n\
            \     - PayTile: cyan\n\
            \     - TaxesTile: yellow\n\
            \     - CareerTile: magenta\n\
            \     - HouseTile: magenta\n\
            \     - TakeTile: yellow\n\
            \     - ActionTile: yellow\n\
            \     - LawsuitTile: blue\n\
            \     - FamilyTile: yellow\n\
            \     - SpintoWinTile: yellow \n\n\
             BlueYellow:\n\
            \     - LifeTile: magenta\n\
            \     - PayTile: green\n\
            \     - TaxesTile: magenta\n\
            \     - CareerTile: red\n\
            \     - HouseTile: red\n\
            \     - TakeTile: magenta\n\
            \     - ActionTile: magenta\n\
            \     - LawsuitTile: white\n\
            \     - FamilyTile: magenta\n\
            \     - SpintoWinTile: magenta\n\
            \ \n\
             GAME PLAY:\n\
            \ \n\
             All players will spin the player by pressing any key when \
             it is their turn. Then, you will move yourself the number \
             of spaces spun. On your first turn, decide to either \
             Start a Career or to Start College. After your turn is \
             finished, the board will print the updated board with the \
             player's new position on the board.\n\
            \            \n\
            \            Career Path: You have the option to pick \
             between two random careers and then will move along that \
             path.\n\
            \ \n\
            \            College Path: Borrow $100,000 from the bank \
             for tuition and you will be in debt for $100,000. \n\
            \            \n\
             As you go through the game you may run into these tiles: \n\
            \            \n\
             Long Term Investments: At the beginning of the game, you \
             may buy one Long-Term Investment card for $10,000 and \
             pick a number associated with it. From now on, whenever \
             any player (including you) spins the number on your \
             Long-Term Investment card, you collect $5,000 from the \
             bank!\n\
            \            \n\
             Card Deck: At the beginning of the game, you will have 3 \
             share the wealth cards. \n\
            \            \n\
             PAY TILE: If you land on a pay tile, you will collect \
             from or pay to the bank the amount of money indicated on \
             the space. \n\
            \            \n\
             LIFE TILE: Each LIFE tile carries a dollar amount that \
             counts towards your total account balance at the end of \
             the game. \n\
            \            \n\
             HOUSE TILE: Take a look at all the available Starter Home \
             cards. Decide which one you want to buy and pay the bank \
             the price on the card. Based on you \n\
            \            \n\
             SPIN TO WIN TILE: \n\
            \            \n\
            \            Land on one of these spaces and you get a \
             chance to win more money! Pick any number to bet upon. \
             Then pay your investment (up to $50,000) to the bank. Now \
             spin the spinner.\n\
            \            \n\
            \            If the number spun matches the number you \
             chose, the bank pays you 10 times the amount you invested.\n\
            \            \n\
            \            If the number spun does not match the number \
             you chose, you lose your investment. Place the token back \
             in the draw pile\n\
            \            \n\
             LAWSUIT TILE: You have the option to sue another player \
             for $10,000 but if they have an exemption card (share the \
             wealth card), then that player is able to deny the \
             lawsuit.\n\
            \             "
        else if x |> normalize_text |> String.equal "no" then
          print_endline ""
        else (
          print_endline "\nInvalid input";
          tut ())
  in

  let print_start () =
    print_endline "\nPlease enter the number of players (2-6).";
    print_string "> "
  in

  let rec int_players () =
    print_start ();
    match int_of_string_opt (String.trim (read_line ())) with
    | Some x ->
        if x > 1 && x < 7 then x
        else (
          print_endline "\nInvalid input ";
          int_players ())
    | None ->
        print_endline " \nInvalid input ";
        int_players ()
  in

  let print_q () =
    print_string "\nDo you want graphics? Input yes or no\n> "
  in
  let rec graphics () =
    print_q ();
    match read_line () with
    | x ->
        if x |> normalize_text |> String.equal "yes" then true
        else if x |> normalize_text |> String.equal "no" then false
        else (
          print_endline "\nInvalid input";
          graphics ())
  in

  let () = tut () in
  let game_players = get_players (int_players ()) [] in
  let final_games_share_wealth =
    share_players game_players share_wealth_cards []
  in
  let deck =
    houses @ careers @ life_tiles @ snd final_games_share_wealth
  in
  let start_state =
    init_state gold_tiles deck
      (fst final_games_share_wealth)
      (graphics ())
  in
  turn start_state

let () = main ()