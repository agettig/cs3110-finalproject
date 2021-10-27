open Source.Gamestate
open Source.Tiles
open Source.Players

open Source.Cards
(** [play_game f] starts the adventure in file [f]. *)

(** [main ()] prompts for the game to play, then starts it. *)

(** [new_player] constructs a new player with a user inputted name and
    whether or not they are going to college*)
let new_player () =
  let () = print_string "Enter Player Name: " in
  let name = read_line () in
  let print_q () =
    print_string "Do you want to to college? Input yes or no\n> "
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

  add_player (String.trim name) (college ())

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