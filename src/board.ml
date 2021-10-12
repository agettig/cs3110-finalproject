(* open Players *)
open Tiles

let spinner = Random.int 10 + 1

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
  | index when index > -1 && index < 130 ->
      (index, List.nth gold_tiles index)
  | index ->
      if index > 0 then
        if index > 129 then raise (Failure "out of bounds position")
        else (129, List.nth gold_tiles 129)
      else raise (Failure "out of bounds position")

(**[make_board] maps each position to a specific binding associated with
   the tile *)
let make_board =
  empty_board
  |> BoardMap.add (pos_to_tuple 0) "|  0  |"
  |> BoardMap.add (pos_to_tuple 1) "|  1  |"
  |> BoardMap.add (pos_to_tuple 2) "|  2  |"
  |> BoardMap.add (pos_to_tuple 3) "|  3  |"
  |> BoardMap.add (pos_to_tuple 4) "|  4  |"
  |> BoardMap.add (pos_to_tuple 5) "|  5  |"
  |> BoardMap.add (pos_to_tuple 6) "|  6  |"
  |> BoardMap.add (pos_to_tuple 7) "|  7  |"
  |> BoardMap.add (pos_to_tuple 8) "|  8  |"
  |> BoardMap.add (pos_to_tuple 9) "|  9  |"
  |> BoardMap.add (pos_to_tuple 10) "|  10 |"
  |> BoardMap.add (pos_to_tuple 11) "|  11 |"
  |> BoardMap.add (pos_to_tuple 12) "|  12 |"
  |> BoardMap.add (pos_to_tuple 13) "|  13 |"
  |> BoardMap.add (pos_to_tuple 14) "|  14 |"
  |> BoardMap.add (pos_to_tuple 15) "|  15 |"
  |> BoardMap.add (pos_to_tuple 16) "|  16 |"
  |> BoardMap.add (pos_to_tuple 17) "|  17 |"
  |> BoardMap.add (pos_to_tuple 18) "|  18 |"
  |> BoardMap.add (pos_to_tuple 19) "|  19 |"
  |> BoardMap.add (pos_to_tuple 20) "|  20 |"
  |> BoardMap.add (pos_to_tuple 21) "|  21 |"
  |> BoardMap.add (pos_to_tuple 22) "|  22 |"
  |> BoardMap.add (pos_to_tuple 23) "|  23 |"
  |> BoardMap.add (pos_to_tuple 24) "|  24 |"
  |> BoardMap.add (pos_to_tuple 25) "|  25 |"
  |> BoardMap.add (pos_to_tuple 26) "|  26 |"
  |> BoardMap.add (pos_to_tuple 27) "|  27 |"
  |> BoardMap.add (pos_to_tuple 28) "|  28 |"
  |> BoardMap.add (pos_to_tuple 29) "|  29 |"
  |> BoardMap.add (pos_to_tuple 30) "|  30 |"
  |> BoardMap.add (pos_to_tuple 31) "|  31 |"
  |> BoardMap.add (pos_to_tuple 32) "|  32 |"
  |> BoardMap.add (pos_to_tuple 33) "|  33 |"
  |> BoardMap.add (pos_to_tuple 34) "|  34 |"
  |> BoardMap.add (pos_to_tuple 35) "|  35 |"
  |> BoardMap.add (pos_to_tuple 36) "|  36 |"
  |> BoardMap.add (pos_to_tuple 37) "|  37 |"
  |> BoardMap.add (pos_to_tuple 38) "|  38 |"
  |> BoardMap.add (pos_to_tuple 39) "|  39 |"
  |> BoardMap.add (pos_to_tuple 40) "|  40 |"
  |> BoardMap.add (pos_to_tuple 41) "|  41 |"
  |> BoardMap.add (pos_to_tuple 42) "|  42 |"
  |> BoardMap.add (pos_to_tuple 43) "|  43 |"
  |> BoardMap.add (pos_to_tuple 44) "|  44 |"
  |> BoardMap.add (pos_to_tuple 45) "|  45 |"
  |> BoardMap.add (pos_to_tuple 46) "|  46 |"
  |> BoardMap.add (pos_to_tuple 47) "|  47 |"
  |> BoardMap.add (pos_to_tuple 48) "|  48 |"
  |> BoardMap.add (pos_to_tuple 49) "|  49 |"
  |> BoardMap.add (pos_to_tuple 50) "|  50 |"
  |> BoardMap.add (pos_to_tuple 51) "|  51 |"
  |> BoardMap.add (pos_to_tuple 52) "|  52 |"
  |> BoardMap.add (pos_to_tuple 53) "|  53 |"
  |> BoardMap.add (pos_to_tuple 54) "|  54 |"
  |> BoardMap.add (pos_to_tuple 55) "|  55 |"
  |> BoardMap.add (pos_to_tuple 56) "|  56 |"
  |> BoardMap.add (pos_to_tuple 57) "|  57 |"
  |> BoardMap.add (pos_to_tuple 58) "|  58 |"
  |> BoardMap.add (pos_to_tuple 59) "|  59 |"
  |> BoardMap.add (pos_to_tuple 60) "|  60 |"
  |> BoardMap.add (pos_to_tuple 61) "|  61 |"
  |> BoardMap.add (pos_to_tuple 62) "|  62 |"
  |> BoardMap.add (pos_to_tuple 63) "|  63 |"
  |> BoardMap.add (pos_to_tuple 64) "|  64 |"
  |> BoardMap.add (pos_to_tuple 65) "|  65 |"
  |> BoardMap.add (pos_to_tuple 66) "|  66 |"
  |> BoardMap.add (pos_to_tuple 67) "|  67 |"
  |> BoardMap.add (pos_to_tuple 68) "|  68 |"
  |> BoardMap.add (pos_to_tuple 69) "|  69 |"
  |> BoardMap.add (pos_to_tuple 70) "|  70 |"
  |> BoardMap.add (pos_to_tuple 71) "|  71 |"
  |> BoardMap.add (pos_to_tuple 72) "|  72 |"
  |> BoardMap.add (pos_to_tuple 73) "|  73 |"
  |> BoardMap.add (pos_to_tuple 74) "|  74 |"
  |> BoardMap.add (pos_to_tuple 75) "|  75 |"
  |> BoardMap.add (pos_to_tuple 76) "|  76 |"
  |> BoardMap.add (pos_to_tuple 77) "|  77 |"
  |> BoardMap.add (pos_to_tuple 78) "|  78 |"
  |> BoardMap.add (pos_to_tuple 79) "|  79 |"
  |> BoardMap.add (pos_to_tuple 80) "|  80 |"
  |> BoardMap.add (pos_to_tuple 81) "|  81 |"
  |> BoardMap.add (pos_to_tuple 82) "|  82 |"
  |> BoardMap.add (pos_to_tuple 83) "|  83 |"
  |> BoardMap.add (pos_to_tuple 84) "|  84 |"
  |> BoardMap.add (pos_to_tuple 85) "|  85 |"
  |> BoardMap.add (pos_to_tuple 86) "|  86 |"
  |> BoardMap.add (pos_to_tuple 87) "|  87 |"
  |> BoardMap.add (pos_to_tuple 88) "|  88 |"
  |> BoardMap.add (pos_to_tuple 89) "|  89 |"
  |> BoardMap.add (pos_to_tuple 90) "|  90 |"
  |> BoardMap.add (pos_to_tuple 91) "|  91 |"
  |> BoardMap.add (pos_to_tuple 92) "|  92 |"
  |> BoardMap.add (pos_to_tuple 93) "|  93 |"
  |> BoardMap.add (pos_to_tuple 94) "|  94 |"
  |> BoardMap.add (pos_to_tuple 95) "|  95 |"
  |> BoardMap.add (pos_to_tuple 96) "|  96 |"
  |> BoardMap.add (pos_to_tuple 97) "|  97 |"
  |> BoardMap.add (pos_to_tuple 98) "|  98 |"
  |> BoardMap.add (pos_to_tuple 99) "|  99 |"
  |> BoardMap.add (pos_to_tuple 100) "| 100 |"
  |> BoardMap.add (pos_to_tuple 101) "| 101 |"
  |> BoardMap.add (pos_to_tuple 102) "| 102 |"
  |> BoardMap.add (pos_to_tuple 103) "| 103 |"
  |> BoardMap.add (pos_to_tuple 104) "| 104 |"
  |> BoardMap.add (pos_to_tuple 105) "| 105 |"
  |> BoardMap.add (pos_to_tuple 106) "| 106 |"
  |> BoardMap.add (pos_to_tuple 107) "| 107 |"
  |> BoardMap.add (pos_to_tuple 108) "| 108 |"
  |> BoardMap.add (pos_to_tuple 109) "| 109 |"
  |> BoardMap.add (pos_to_tuple 110) "| 110 |"
  |> BoardMap.add (pos_to_tuple 111) "| 111 |"
  |> BoardMap.add (pos_to_tuple 112) "| 112 |"
  |> BoardMap.add (pos_to_tuple 113) "| 113 |"
  |> BoardMap.add (pos_to_tuple 114) "| 114 |"
  |> BoardMap.add (pos_to_tuple 115) "| 115 |"
  |> BoardMap.add (pos_to_tuple 116) "| 116 |"
  |> BoardMap.add (pos_to_tuple 117) "| 117 |"
  |> BoardMap.add (pos_to_tuple 118) "| 118 |"
  |> BoardMap.add (pos_to_tuple 119) "| 119 |"
  |> BoardMap.add (pos_to_tuple 120) "| 120 |"
  |> BoardMap.add (pos_to_tuple 121) "| 121 |"
  |> BoardMap.add (pos_to_tuple 122) "| 122 |"
  |> BoardMap.add (pos_to_tuple 123) "| 123 |"
  |> BoardMap.add (pos_to_tuple 124) "| 124 |"
  |> BoardMap.add (pos_to_tuple 125) "| 125 |"
  |> BoardMap.add (pos_to_tuple 126) "| 126 |"
  |> BoardMap.add (pos_to_tuple 127) "| 127 |"
  |> BoardMap.add (pos_to_tuple 128) "| 128 |"
  |> BoardMap.add (pos_to_tuple 129) "| 129 |"

let make_color pos board =
  match pos with
  | pos ->
      ANSITerminal.(
        print_string [ white; on_black ] (BoardMap.find pos board))

let rec init_board pos board =
  match pos with
  | pos ->
      if not (BoardMap.mem (pos_to_tuple pos) board) then
        failwith "invalid position"
      else make_color (pos_to_tuple pos) board;
      init_board (pos + 1) board
