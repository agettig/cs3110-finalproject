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
  let () = print_string "\nEnter player name: " in
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
  let init_player = add_player (String.trim name) (college ()) in
  let print_q2 () =
    print_string
      "Do you want to buy a long term investment? Input yes or no \n > "
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
        if x |> String.trim |> String.equal "yes" then num ()
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
            "\n\
             Two players cannot have identical long term investments - \
             restart creation of this player"
        in
        get_players h acc
      else get_players (h - 1) (acc @ [ newbie ])

let init_state tiles deck players =
  { tiles; deck; current_player = List.nth players 0; players }

(* ------start of random print testing *)
let random_money_str () = if Random.int 2 = 1 then "$" else " "

let rec make_it_rain (num : int) (acc : string) =
  match num with
  | 0 -> print_endline acc
  | _ -> make_it_rain (num - 1) (acc ^ random_money_str ())

let rec make_it_rain_iter (num : int) (idx : int) =
  match idx with
  | 0 -> make_it_rain num ""
  | _ ->
      make_it_rain num "";
      Unix.sleepf 0.15;
      make_it_rain_iter num (idx - 1)

(* -------end of random print testing *)
let main () =
  (* let () = make_it_rain_iter 30 50 in *)
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