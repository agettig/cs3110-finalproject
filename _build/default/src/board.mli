open Tiles

module BoardMap : sig
  type 'a t
end

module IntTilesTupl : sig
  type t

  val compare : 'a -> 'a -> int
end

type colorblind =
  | Not
  | RedGreen
  | BlueYellow

val spinner : unit -> int
(**Creates the spinner value at the beginning of a turn*)

val empty_board : 'a BoardMap.t
(**Creates an empty instance of BoardMap*)

val pos_to_tuple : int -> IntTilesTupl.t
(**[pos_to_tuple index_on_board] maps the position index
   [index_on_board] to the specific tile that the player is on *)

val make_board : string BoardMap.t
(**[make_board] maps each position to a specific binding associated with
   the tile*)

val make_color : int * tiles -> string BoardMap.t -> colorblind -> unit
(**[make_color] prints the board in the color setting [color_op] based
   on [pos] and the associated binding on the board[board]*)

val print_color_tile : int -> string BoardMap.t -> colorblind -> unit
(**[print_color_tile] prints the tile associated with [pos] in [board]*)

val init_board : int -> string BoardMap.t -> colorblind -> unit
(**[init_board] initializes each position of the board and prepares them
   to be printed to the terminal*)

val update_board :
  'a BoardMap.t -> (IntTilesTupl.t * 'a) list -> 'a BoardMap.t
(**[update_board] updates each player's position from the
   list[updated_player_lst] onto the current board [board] and prepares
   the new board to be printed to the terminal*)

val print_board : string BoardMap.t -> colorblind -> unit
(**[print_board] prints the current board [board] to the terminal*)