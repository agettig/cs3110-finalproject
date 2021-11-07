open Board
open Tiles

(**Creating and printing the board to the terminal*)

val spinner : unit -> int
(**Creates the spinner value at the beginning of a turn*)

module type IntTilesTupl = sig
  type t

  val compare : 'a -> 'a -> int
end

val empty_board : 'a BoardMap.t
(**Creates an empty instance of BoardMap*)

val pos_to_tuple : int -> int * tiles
(**[pos_to_tuple index_on_board] maps the position index to the specific
   tile that the player is on *)

val make_board : string BoardMap.t
(**[make_board] maps each position to a specific binding associated with
   the tile*)

val make_color : int * tiles -> string BoardMap.t -> unit
(**[make_color] prints the board in color based on [pos] and the
   associated binding [board]*)

val print_color_tile : int -> string BoardMap.t -> unit
(**[print_color_tile] prints the tile associated with [pos] in [board]*)

val init_board : int -> string BoardMap.t -> unit
(**[init_board] initializes each position of the board and prepares them
   to be printed to the terminal*)

val update_board :
  'a BoardMap.t -> (IntTilesTupl.t * 'a) list -> 'a BoardMap.t

val print_board : string BoardMap.t -> unit
