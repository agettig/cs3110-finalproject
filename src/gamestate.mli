open Players
open Tiles
open Cards

type gamestate = {
  current_player : player;
  players : player list;
  tiles : tiles list;
  deck : cards list;
  graphics : bool;
}

val turn : gamestate -> unit
(**[turn st] prints the winner of the Game to terminal *)

val player_winner : player list -> player -> player
(** [player_winner lst player] returns the winner of the game. Requires
    the game is over *)

val gameover : player list -> bool
(** [gamover lst] returns true if the game is over else false *)

val normalize_text : string -> string
(** [normalize_text s] returns s with whitespace trimmed and in all
    lowercase *)

val print_iter : (int -> unit) -> int -> int -> bool -> unit
(** [print_iter pfun acc cap graphic] prints [pfun] if [graphic] is true
    else unit is returned *)

val get_tile : int -> tiles
(** [get_tile index] returns the tile in [tiles] at given index [index].
    Raises : [Not_found] if [tile] does not exist in [int_tiles] *)

val next_player : player -> player list -> player
(** [next_player current_player players] returns the player whose turn
    is after [current_player] *)

val finished : player -> bool
(** [finished player] returns true if the player has reached the end of
    the board and has retired and returns false if player is still
    playing. *)

val change_index_board : player -> player * int
(** [change_index_board p] returns a pair of the player with an updated
    location on the board and the number that they spun *)

val final_tile_index : int
(** [ final_tile_index] is the index of the last tile on the board *)

val new_deck_helper :
  cards option * cards option -> gamestate -> cards list
(** [new_deck_helper cards_option gamestate] returns an updated deck by
    removing cards given to players and inserting cards discarded by
    players *)
