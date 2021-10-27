open Source.Gamestate
open Source.Tiles
open Source.Players
open Source.Bank

open Source.Cards
(** [play_game f] starts the adventure in file [f]. *)

(** [main ()] prompts for the game to play, then starts it. *)

(** [new_player] constructs a new player with a user inputted name and
    whether or not they are going to college*)
let new_player () =
  let () = print_string "Enter player name: " in
  let name = read_line () in
  let print_q () =
    print_string "Do you want to to college? Input yes or no\n> "
  in
  let rec college () =
    print_q ();
    match read_line () with
    | x ->
        if x |> String.trim |> String.equal "yes" then true
        else if x |> String.trim |> String.equal "no" then false
        else (
          print_endline "\nInvalid input";
          college ())
  in
  let init_player = add_player (String.trim name) (college ()) in
  let print_q2 () =
    print_string
      "Do you want to buy a long term investment? Input yes or no \n > "
  in
  let rec buy () =
    print_q2 ();
    match read_line () with
    | x ->
        if x |> String.trim |> String.equal "yes" then
          (* check if it is a number 1 thru 10 *)
          let () = print_string "Enter a number 1 through 10: " in
          let num = read_line () in
          add_balance
            (add_card
               (List.nth lg_tm_invt (int_of_string num - 1))
               init_player)
            (-1 * 10000)
        else if x |> String.trim |> String.equal "no" then init_player
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
            "\nTwo players cannot have identical long term investments"
        in
        get_players h acc
      else get_players (h - 1) (acc @ [ newbie ])

let init_state tiles deck players =
  { tiles; deck; current_player = List.nth players 0; players }

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
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
  let game_players = get_players (int_players ()) [] in
  let start_state =
    init_state gold_tiles (houses @ careers @ life_tiles) game_players
  in
  turn start_state

(* let player_lst = get_players (int_of_string num_players) [] in turn
   (init_state gold_tiles (houses @ careers @ life_tiles) player_lst) *)
(* Execute the game engine. *)
let () = main ()