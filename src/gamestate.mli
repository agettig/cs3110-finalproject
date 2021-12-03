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
(** [player_winner lst] returns the winner of the game. Requires the
    game is over *)

val gameover : player list -> bool
(** [gamover lst] returns true if the game is over else false *)

val normalize_text : string -> string
(** [normalize_text s] returns s with whitespace trimmed and all
    lowercase *)

val print_iter : (int -> unit) -> int -> int -> bool -> unit
(** [print_iter pfun acc cap graphic] prints [pfun] if [graphic] is true
    else unit is returned *)

val get_tile : int -> tiles
(** [get_tile] returns the tile in [tiles] at given index [index].
    Raises : Failure if list is too short and Invalid Argument if n is
    negative. *)

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

val next_player : player -> player list -> player
(** [next_player current_player players] returns player whose turn is
    after [current_player] in [players] *)

val has_spin_card : cards list -> cards option
(** [has_career deck] returns Some SpinToWin card if there is a
    SpinToWin card in [deck], else None*)

val has_exemption_card : cards list -> cards option
(** [has_exemption_card deck] returns Some exemption card if there is an
    exemption card in [deck], else None *)

val num_of_guesses_helper : cards option -> int
(** [num_of_guesses_helper c] returns 1 if c is None or returns Some x
    where x is in SpinToWin_Card (x). Raises Failure "Illegal" if c is
    not None or a Spin To Win Card *)

val new_deck_helper :
  cards option * cards option -> gamestate -> cards list
(** [new_deck_helper cards_option gamestate] returns an updated deck by
    removing cards given to players and inserting cards discarded by
    players *)

val life_cards_in_deck : cards list -> cards list -> cards list
(** [life_cards_in_deck acc deck] returns [acc] with all life tile cards
    in [deck] added *)

val new_player_helper :
  tiles ->
  player ->
  gamestate ->
  player list * (cards option * cards option)
(* [new_player] returns a tuple with an updated players list and a pair
   of card options. The first entry in the card pair is a card that
   needs to be removed from the gamedeck. The second entry are cards
   that need to be added to the gamedeck. If None then the deck remains
   the same. If Some x, then x is added to of removed from the deck
   corresponding to its entry. *)