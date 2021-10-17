open Players
open Tiles
open Cards
open Bank
open Board

type gamestate = {
  current_player : player;
  players : player list;
  tiles : tiles list;
  deck : cards list;
}

exception Invalid_Input

(** [num_players] equals the number of players in the game*)
let num_players =
  let () = print_string "Enter number of players: " in
  read_int ()

(** [new_player] constructs a new player with a user inputted name and
    whether or not they are going to college*)
let new_player () =
  let () = print_string "Enter Player Name: " in
  let name = read_line () in
  let () = print_string "Do you want to to college? Input yes or no " in
  let college = read_line () in
  let bool_college =
    if String.equal college "yes" then true
    else if String.equal college "no" then false
    else failwith "invalid input"
  in
  add_player name bool_college

(** [get_players number_players acc] recursively constructs the list of
    players in the game*)
let rec get_players num_players acc =
  match num_players with
  | 0 -> acc
  | h -> get_players (h - 1) (acc @ [ new_player () ])

(** [players] represents the players in life. *)
let players = get_players num_players []

let init_state tiles deck players =
  { tiles; deck; current_player = List.nth players 0; players }

(** [current_player gamestate] is the identifier of the player whose
    turn it is in which the in gamestate [st]. *)
let current_player st = st.current_player

(** [final_tile_index] is the last tile on the path of life which
    signifies retirement*)
let final_tile_index = 130

(** [index_in_list_helper player lst c] returns an int that represents
    the index of [player] in list [lst]. Raises: Failswith "Not found"
    if [player] is not in [lst].*)
let rec index_in_list_helper player lst c =
  match lst with
  | [] -> failwith "Not found"
  | h :: t ->
      if h = player then c else index_in_list_helper player t (c + 1)

(** [index_in_list_next] returns the index of the next player after
    [player]. Postcondition: [index_in_lst] returns an int between 0 and
    (list length -1)*)
let index_in_list_next (player : player) (lst : player list) : int =
  (index_in_list_helper player lst 0 + 1) mod List.length lst

(** [next_player current_player players] returns the player whose turn
    is after [current_player]*)
let next_player current_player players =
  List.nth players (index_in_list_next current_player players)

(** [finished player] returns true if the player has reached the end of
    the board and has retired and returns false if player is still
    playing.*)
let finished player = player.index_on_board >= final_tile_index

(** [get_tile] returns the tile in [tiles] at given index [index].
    Raises : Failure if list is too short and Invalid Argument if n is
    negative. *)
let get_tile index tiles : tiles = List.nth tiles index

(** [change_index_board player] returns the new position of the player
    after they move a given number of spaces determined by the spinner*)
let married_index, starter_home_index, house_index, retirement, elope =
  (25, 33, 97, 130, 20)

(** [change_index_board player] returns a player with their new position
    after that have spun the spinner and moved appropriately*)
let change_index_board (player : player) : player =
  let current_index = player.index_on_board in
  let spinner = spinner () in
  (* player position before adjustment*)
  let player_index_spinner = current_index + spinner in
  let new_index =
    (* makes college player stop at tile 10 to get college career*)
    if player.college && current_index < 10 then
      if player_index_spinner > 10 then 10
      else current_index
        (* maintains order for college player jumping from tile 10 to
           tile 15 to stay on main path *)
    else if player.college && current_index = 10 then
      4 + player_index_spinner
      (* makes each player stop at the marriage tile *)
    else if current_index < married_index then
      if player_index_spinner > married_index then married_index
      else player_index_spinner
        (* makes each player stop to buy a starter home *)
    else if
      current_index >= married_index
      && current_index < starter_home_index
    then
      if player_index_spinner > starter_home_index then
        starter_home_index
      else player_index_spinner
        (* makes each player stop to buy a house *)
    else if
      current_index >= starter_home_index && current_index < house_index
    then
      if player_index_spinner > house_index then house_index
      else player_index_spinner
        (* makes sure player cannot make illegal moves past final
           tile *)
    else if player_index_spinner > 130 then 130
    else player_index_spinner
  in
  { player with index_on_board = new_index }

(** [new_players_lst] returns an updated player with the current players
    record updated*)
let new_players_list current_lst updated_player =
  List.map
    (fun x ->
      if x.name = updated_player.name then updated_player else x)
    current_lst

let turn gamestate : gamestate =
  (*player with new index*)
  let player_moved = change_index_board gamestate.current_player in

  (*tile on which [player_moved] is on*)
  let tile = get_tile player_moved.index_on_board [] in

  (* [new_player] returns a tuple with an updated player and None if the
     game deck does not need to be altered and Some card if card has to
     be removed from the game deck*)
  let new_player : player * cards option =
    match tile with
    | PayTile c -> (add_balance player_moved c.account_change, None)
    | TaxesTile _ ->
        (add_balance player_moved (-1 * get_taxes player_moved), None)
    | LifeTile _ ->
        let rand_lf_tile =
          List.nth life_tiles (Random.int (List.length life_tiles))
        in
        (add_card rand_lf_tile player_moved, Some rand_lf_tile)
    | CareerTile _ -> (player_moved, None)
    | FamilyTile c ->
        if c.index_tile = married_index then
          ({ player_moved with so = true }, None)
        else if c.index_tile = elope then
          ( {
              player_moved with
              so = true;
              index_on_board = married_index;
            },
            None )
        else
          ( {
              player_moved with
              children = player_moved.children (*+ c.children *);
            },
            None )
    | HouseTile _ -> (player_moved, None)
    | TakeTile _ -> (player_moved, None) (* not implemented in ms1*)
    | ActionTile _ -> (player_moved, None)
    | SpinToWinTile _ ->
        (player_moved, None) (* not implemented in ms1*)
    | LawsuitTile _ -> (player_moved, None)
    (* not implemented in ms1*)
  in
  (*[new_play_list] is the updated player list after the current
    player's turn*)
  let new_play_list =
    new_players_list gamestate.players (fst new_player)
  in
  (* [new_deck] is the new game deck*)
  let new_deck =
    match snd new_player with
    | None -> gamestate.deck
    | Some x -> remove_from_deck gamestate.deck x []
  in
  (* returns new gamestate with updated records*)
  {
    gamestate with
    current_player =
      next_player gamestate.current_player gamestate.players;
    players = new_play_list;
    deck = new_deck;
  }

(** [gameover players] returns true if all players in the game have
    retired and returns false if anyone is still playing.*)
let rec gameover players =
  match players with
  | [] -> true
  | h :: t -> if not (finished h) then false else gameover t

(** [player_winner player_lst player] returns the player who has won the
    game with the largest account balance at the end of the game.*)
let rec player_winner player_lst player =
  match player_lst with
  | [] -> player
  | h :: t ->
      if final_balance h > final_balance player then player_winner t h
      else player_winner t player
