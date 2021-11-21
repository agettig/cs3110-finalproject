open Source.Gamestate
open Source.Players
open Source.Bank
open Source.Cards
open Source.Tiles

open Source.Printing
(** [play_game f] starts the adventure in file [f]. *)

(** [main ()] prompts for the game to play, then starts it. *)

(** [new_player] constructs a new player with a user inputted name and
    whether or not they are going to college*)

let new_player () =
  let () = print_string "\nEnter player name: " in
  let name = read_line () in
  let print_q () =
    print_string "Do you want to go to college? Input yes or no\n> "
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
  let init_player = add_player (String.trim name) (college ()) in
  let print_q2 () =
    print_string
      "Do you want to buy a long term investment? Input yes or no \n> "
  in
  let print_q3 () = print_string "Enter a number 1 through 10: " in
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
  let print_start () =
    print_endline "Please enter the number of players (2-6).";
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