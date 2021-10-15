(* open Players *)
open Tiles

let spinner () = Random.int 10 + 1

module IntTilesTupl = struct
  type t = int * tiles

  let compare pos1 pos2 =
    match Stdlib.compare pos1 pos2 with
    | 0 -> 0
    | comp -> comp
end

module BoardMap = Map.Make (IntTilesTupl)

let empty_board = BoardMap.empty

(**[pos_to_tuple index_on_board] maps the position index to the specific
   tile that the player is on *)
let pos_to_tuple index_on_board =
  match index_on_board with
  | index ->
      if index > -1 && index < 129 then
        (index, List.nth gold_tiles index)
      else failwith "out of bounds position"

(**[make_board] maps each position to a specific binding associated with
   the tile *)
let make_board =
  empty_board
  |> BoardMap.add (pos_to_tuple 0) "|  1  |"
  |> BoardMap.add (pos_to_tuple 1) "|  2  |"
  |> BoardMap.add (pos_to_tuple 2) "|  3  |"
  |> BoardMap.add (pos_to_tuple 3) "|  4  |"
  |> BoardMap.add (pos_to_tuple 4) "|  5  |"
  |> BoardMap.add (pos_to_tuple 5) "|  6  |"
  |> BoardMap.add (pos_to_tuple 6) "|  7  |"
  |> BoardMap.add (pos_to_tuple 7) "|  8  |"
  |> BoardMap.add (pos_to_tuple 8) "|  9  |"
  |> BoardMap.add (pos_to_tuple 9) "| 10 |"
  |> BoardMap.add (pos_to_tuple 10) "| 11 |"
  |> BoardMap.add (pos_to_tuple 11) "| 12 |"
  |> BoardMap.add (pos_to_tuple 12) "| 13 |"
  |> BoardMap.add (pos_to_tuple 13) "| 14 |"
  |> BoardMap.add (pos_to_tuple 14) "| 15 |"
  |> BoardMap.add (pos_to_tuple 15) "| 16 |"
  |> BoardMap.add (pos_to_tuple 16) "| 17 |"
  |> BoardMap.add (pos_to_tuple 17) "| 18 |"
  |> BoardMap.add (pos_to_tuple 18) "| 19 |"
  |> BoardMap.add (pos_to_tuple 19) "| 20 |"
  |> BoardMap.add (pos_to_tuple 20) "| 21 |"
  |> BoardMap.add (pos_to_tuple 21) "| 22 |"
  |> BoardMap.add (pos_to_tuple 22) "| 23 |"
  |> BoardMap.add (pos_to_tuple 23) "| 24 |"
  |> BoardMap.add (pos_to_tuple 24) "| 25 |"
  |> BoardMap.add (pos_to_tuple 25) "| 26 |"
  |> BoardMap.add (pos_to_tuple 26) "| 27 |"
  |> BoardMap.add (pos_to_tuple 27) "| 28 |"
  |> BoardMap.add (pos_to_tuple 28) "| 29 |"
  |> BoardMap.add (pos_to_tuple 29) "| 30 |"
  |> BoardMap.add (pos_to_tuple 30) "| 31 |"
  |> BoardMap.add (pos_to_tuple 31) "| 32 |"
  |> BoardMap.add (pos_to_tuple 32) "| 33 |"
  |> BoardMap.add (pos_to_tuple 33) "| 34 |"
  |> BoardMap.add (pos_to_tuple 34) "| 35 |"
  |> BoardMap.add (pos_to_tuple 35) "| 36 |"
  |> BoardMap.add (pos_to_tuple 36) "| 37 |"
  |> BoardMap.add (pos_to_tuple 37) "| 38 |"
  |> BoardMap.add (pos_to_tuple 38) "| 39 |"
  |> BoardMap.add (pos_to_tuple 39) "| 40 |"
  |> BoardMap.add (pos_to_tuple 40) "| 41 |"
  |> BoardMap.add (pos_to_tuple 41) "| 42 |"
  |> BoardMap.add (pos_to_tuple 42) "| 43 |"
  |> BoardMap.add (pos_to_tuple 43) "| 44 |"
  |> BoardMap.add (pos_to_tuple 44) "| 45 |"
  |> BoardMap.add (pos_to_tuple 45) "| 46 |"
  |> BoardMap.add (pos_to_tuple 46) "| 47 |"
  |> BoardMap.add (pos_to_tuple 47) "| 48 |"
  |> BoardMap.add (pos_to_tuple 48) "| 49 |"
  |> BoardMap.add (pos_to_tuple 49) "| 50 |"
  |> BoardMap.add (pos_to_tuple 50) "| 51 |"
  |> BoardMap.add (pos_to_tuple 51) "| 52 |"
  |> BoardMap.add (pos_to_tuple 52) "| 53 |"
  |> BoardMap.add (pos_to_tuple 53) "| 54 |"
  |> BoardMap.add (pos_to_tuple 54) "| 55 |"
  |> BoardMap.add (pos_to_tuple 55) "| 56 |"
  |> BoardMap.add (pos_to_tuple 56) "| 57 |"
  |> BoardMap.add (pos_to_tuple 57) "| 58 |"
  |> BoardMap.add (pos_to_tuple 58) "| 59 |"
  |> BoardMap.add (pos_to_tuple 59) "| 60 |"
  |> BoardMap.add (pos_to_tuple 60) "| 61 |"
  |> BoardMap.add (pos_to_tuple 61) "| 62 |"
  |> BoardMap.add (pos_to_tuple 62) "| 63 |"
  |> BoardMap.add (pos_to_tuple 63) "| 64 |"
  |> BoardMap.add (pos_to_tuple 64) "| 65 |"
  |> BoardMap.add (pos_to_tuple 65) "| 66 |"
  |> BoardMap.add (pos_to_tuple 66) "| 67 |"
  |> BoardMap.add (pos_to_tuple 67) "| 68 |"
  |> BoardMap.add (pos_to_tuple 68) "| 69 |"
  |> BoardMap.add (pos_to_tuple 69) "| 70 |"
  |> BoardMap.add (pos_to_tuple 70) "| 71 |"
  |> BoardMap.add (pos_to_tuple 71) "| 72 |"
  |> BoardMap.add (pos_to_tuple 72) "| 73 |"
  |> BoardMap.add (pos_to_tuple 73) "| 74 |"
  |> BoardMap.add (pos_to_tuple 74) "| 75 |"
  |> BoardMap.add (pos_to_tuple 75) "| 76 |"
  |> BoardMap.add (pos_to_tuple 76) "| 77 |"
  |> BoardMap.add (pos_to_tuple 77) "| 78 |"
  |> BoardMap.add (pos_to_tuple 78) "| 79 |"
  |> BoardMap.add (pos_to_tuple 79) "| 80 |"
  |> BoardMap.add (pos_to_tuple 80) "| 81 |"
  |> BoardMap.add (pos_to_tuple 81) "| 82 |"
  |> BoardMap.add (pos_to_tuple 82) "| 83 |"
  |> BoardMap.add (pos_to_tuple 83) "| 84 |"
  |> BoardMap.add (pos_to_tuple 84) "| 85 |"
  |> BoardMap.add (pos_to_tuple 85) "| 86 |"
  |> BoardMap.add (pos_to_tuple 86) "| 87 |"
  |> BoardMap.add (pos_to_tuple 87) "| 88 |"
  |> BoardMap.add (pos_to_tuple 88) "| 89 |"
  |> BoardMap.add (pos_to_tuple 89) "| 90 |"
  |> BoardMap.add (pos_to_tuple 90) "| 91 |"
  |> BoardMap.add (pos_to_tuple 91) "| 92 |"
  |> BoardMap.add (pos_to_tuple 92) "| 93 |"
  |> BoardMap.add (pos_to_tuple 93) "| 94 |"
  |> BoardMap.add (pos_to_tuple 94) "| 95 |"
  |> BoardMap.add (pos_to_tuple 95) "| 96 |"
  |> BoardMap.add (pos_to_tuple 96) "| 97 |"
  |> BoardMap.add (pos_to_tuple 97) "| 98 |"
  |> BoardMap.add (pos_to_tuple 98) "| 99 |"
  |> BoardMap.add (pos_to_tuple 99) "| 100 |"
  |> BoardMap.add (pos_to_tuple 100) "| 101 |"
  |> BoardMap.add (pos_to_tuple 101) "| 102 |"
  |> BoardMap.add (pos_to_tuple 102) "| 103 |"
  |> BoardMap.add (pos_to_tuple 103) "| 104 |"
  |> BoardMap.add (pos_to_tuple 104) "| 105 |"
  |> BoardMap.add (pos_to_tuple 105) "| 106 |"
  |> BoardMap.add (pos_to_tuple 106) "| 107 |"
  |> BoardMap.add (pos_to_tuple 107) "| 108 |"
  |> BoardMap.add (pos_to_tuple 108) "| 109 |"
  |> BoardMap.add (pos_to_tuple 109) "| 110 |"
  |> BoardMap.add (pos_to_tuple 110) "| 111 |"
  |> BoardMap.add (pos_to_tuple 111) "| 112 |"
  |> BoardMap.add (pos_to_tuple 112) "| 113 |"
  |> BoardMap.add (pos_to_tuple 113) "| 114 |"
  |> BoardMap.add (pos_to_tuple 114) "| 115 |"
  |> BoardMap.add (pos_to_tuple 115) "| 116 |"
  |> BoardMap.add (pos_to_tuple 116) "| 117 |"
  |> BoardMap.add (pos_to_tuple 117) "| 118 |"
  |> BoardMap.add (pos_to_tuple 118) "| 119 |"
  |> BoardMap.add (pos_to_tuple 119) "| 120 |"
  |> BoardMap.add (pos_to_tuple 120) "| 121 |"
  |> BoardMap.add (pos_to_tuple 121) "| 122 |"
  |> BoardMap.add (pos_to_tuple 122) "| 123 |"
  |> BoardMap.add (pos_to_tuple 123) "| 124 |"
  |> BoardMap.add (pos_to_tuple 124) "| 125 |"
  |> BoardMap.add (pos_to_tuple 125) "| 126 |"
  |> BoardMap.add (pos_to_tuple 126) "| 127 |"
  |> BoardMap.add (pos_to_tuple 127) "| 128 |"
  |> BoardMap.add (pos_to_tuple 128) "| 129 |"

(**[make_color] prints the board in color based on [pos] and the
   associated binding [board]*)
let make_color pos board =
  match pos with
  | index, _ when index mod 30 = 0 ->
      print_endline "";
      ANSITerminal.(
        print_string [ white; on_black ] (BoardMap.find pos board))
  | pos ->
      ANSITerminal.(
        print_string [ white; on_black ] (BoardMap.find pos board))

(**[init_board] initializes each position of the board and prepares them
   to be printed to the terminal*)
let rec init_board pos board =
  match pos with
  | pos when pos = 129 -> print_endline ""
  | pos ->
      if not (pos > -1 && pos < 129) then failwith "invalid position"
      else if BoardMap.mem (pos_to_tuple pos) board then
        make_color (pos_to_tuple pos) board;
      init_board (pos + 1) board

(*let rec update_board pos board =*)

(**[print_board] prints the initialized board to the terminal*)
let print_board = init_board 0 make_board
