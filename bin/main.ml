(* open Source.Gamestate *)
(** [play_game f] starts the adventure in file [f]. *)

(** [main ()] prompts for the game to play, then starts it. *)

(* let initiate_game num_players = *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Game of Life.\n";
  print_endline "Please enter the number of players (2-6).\n";
  print_string "> "

(* match read_line () with | exception End_of_file -> () | num_players
   -> initiate_game num_players *)
(* Execute the game engine. *)
let () = main ()