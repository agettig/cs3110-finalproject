open Source.Gamestate
open Source.Tiles
open Source.Players

open Source.Cards
(** [play_game f] starts the adventure in file [f]. *)

(** [main ()] prompts for the game to play, then starts it. *)

(** [new_player] constructs a new player with a user inputted name and
    whether or not they are going to college*)
let new_player () =
  let () = print_string "Enter player name: " in
  let name = read_line () in
  let () =
    print_string "Do you want to go to college? Input yes or no \n > "
  in
  let college = read_line () in
  let bool_college =
    if String.equal college "yes" then true
    else if String.equal college "no" then false
    else failwith "invalid input"
  in
  let init_player = add_player name bool_college in
  let () =
    print_string
      "Do you want to buy a long term investment? Input yes or no \n > "
  in
  let buy = read_line () in
  if buy = "no" then init_player
  else if buy = "yes" then
    let () = print_string "Enter a number 1 through 10: " in
    let num = read_line () in
    add_card (List.nth lg_tm_invt (int_of_string num - 1)) init_player
  else failwith "invalid input"

(** [get_players number_players acc] recursively constructs the list of
    players in the game*)
let rec get_players num_players acc =
  match num_players with
  | 0 -> acc
  | h -> get_players (h - 1) (acc @ [ new_player () ])

let init_state tiles deck players =
  { tiles; deck; current_player = List.nth players 0; players }

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Game of Life.\n";
  print_endline "Please enter the number of players (2-6).\n";
  print_string "> ";
  let num_players = read_line () in
  let game_players = get_players (int_of_string num_players) [] in
  let start_state =
    init_state gold_tiles (houses @ careers @ life_tiles) game_players
  in
  turn start_state

(* let player_lst = get_players (int_of_string num_players) [] in turn
   (init_state gold_tiles (houses @ careers @ life_tiles) player_lst) *)
(* Execute the game engine. *)
let () = main ()