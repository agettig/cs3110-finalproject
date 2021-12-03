let print_payday (num : int) =
  match num with
  | 0 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$                  $$$$$$$$$$$              \
         $$$$$             $$$$$      \
         $$$$$$$$$$$$$$$$$$$                       \
         $$$$$$$$$$$              $$$$$             $$$$$"
  | 1 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$$$               \
         $$$$$$$$$$$$$              $$$$$           $$$$$       \
         $$$$$$$$$$$$$$$$$$$$$                    \
         $$$$$$$$$$$$$              $$$$$           $$$$$ "
  | 2 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$$$$             \
         $$$$$$$$$$$$$$$              $$$$$         $$$$$        \
         $$$$$$$$$$$$$$$$$$$$$$                  \
         $$$$$$$$$$$$$$$              $$$$$         $$$$$  "
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$             $$$$$           $$$$$       \
         $$$$$              $$$$$       $$$$$         $$$$$            \
         $$$$$$                $$$$$       $$$$$              \
         $$$$$       $$$$$   "
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$              $$$$          $$$$$         \
         $$$$$              $$$$$     $$$$$          $$$$$             \
         $$$$$$              $$$$$         $$$$$              \
         $$$$$     $$$$$    "
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$              $$$$         $$$$$           \
         $$$$$              $$$$$   $$$$$           $$$$$              \
         $$$$$             $$$$$           $$$$$              $$$$$   \
         $$$$$     "
  | 6 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$             $$$$$        \
         $$$$$$$$$$$$$$$$$$$$$$$              $$$$$ $$$$$            \
         $$$$$              $$$$$            \
         $$$$$$$$$$$$$$$$$$$$$$$              $$$$$ $$$$$      "
  | 7 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$$$         \
         $$$$$$$$$$$$$$$$$$$$$$$$$              $$$$$$$$$             \
         $$$$$              $$$$$           \
         $$$$$$$$$$$$$$$$$$$$$$$$$              $$$$$$$$$       "
  | 8 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$$$$$        \
         $$$$$$$$$$$$$$$$$$$$$$$$$$$              $$$$$$$              \
         $$$$$              $$$$$          \
         $$$$$$$$$$$$$$$$$$$$$$$$$$$              $$$$$$$        "
  | 9 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$$$$$$$$$$$$$           $$$$$                   \
         $$$$$              $$$$$               $$$$$              \
         $$$$$         $$$$$                   $$$$$              \
         $$$$$         "
  | 10 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                      $$$$$                     \
         $$$$$             $$$$$               $$$$$             \
         $$$$$$        $$$$$                     $$$$$             \
         $$$$$       "
  | 11 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                     $$$$$                       \
         $$$$$            $$$$$               $$$$$            \
         $$$$$$        $$$$$                       $$$$$            \
         $$$$$       "
  | 12 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                    $$$$$                         \
         $$$$$           $$$$$               \
         $$$$$$$$$$$$$$$$$$$$$$        $$$$$                         \
         $$$$$           $$$$$       "
  | 13 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                   $$$$$                           \
         $$$$$          $$$$$               \
         $$$$$$$$$$$$$$$$$$$$$        $$$$$                           \
         $$$$$          $$$$$       "
  | 14 ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "$$$$$                  $$$$$                             \
         $$$$$         $$$$$               $$$$$$$$$$$$$$$$$$$         \
         $$$$$                             $$$$$         $$$$$       \n"
  | _ -> print_endline ""

let print_wedding (num : int) =
  match num with
  | 0 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "                         --."
  | 1 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "          @@@           /  ))"
  | 2 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "         (( }           7_ /"
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "         ``)             / \\"
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "        ``( \\           |<| |"
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "       ```\\`.\\__     __/|/| |"
  | 6 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "      ```` )|---~o_)|___| | |"
  | 7 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "     ~~~~~/ \\`          | |_|"
  | 8 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "         / ' \\`         |__>)"
  | 9 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "        /  '  \\`        || |"
  | 10 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "       /   '    `       |\\ \\"
  | 11 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "      /   _!__.-._`     | \\ \\"
  | 12 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "     /_.-'  ( |         | |\\ \\"
  | 13 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "        /    `|         |_| \\_\\"
  | 14 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "       |`_   |`_      __'_)__.-'"
  | _ -> print_endline ""

let print_life (num : int) =
  match num with
  | 0 ->
      ANSITerminal.print_string
        [ ANSITerminal.magenta ]
        "███████████████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "████████████";
      ANSITerminal.print_string [ ANSITerminal.green ]
        "████████████████████";
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "███████████████████"
  | 1 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██"
  | 2 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██"
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██████████"
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██████████"
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██"
  | 6 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██"
  | 7 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██████████"
  | 8 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██████████"
  | 9 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██"
  | 10 ->
      ANSITerminal.print_string [ ANSITerminal.magenta ] "██";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.magenta ] "███";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.blue ] "███";
      ANSITerminal.print_string [ ANSITerminal.green ] "███";
      ANSITerminal.print_string [ ANSITerminal.on_white ] "      ";
      ANSITerminal.print_string [ ANSITerminal.green ] "███████████";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "███";
      ANSITerminal.print_string
        [ ANSITerminal.on_white ]
        "              ";
      ANSITerminal.print_string [ ANSITerminal.yellow ] "██"
  | 11 ->
      ANSITerminal.print_string
        [ ANSITerminal.magenta ]
        "███████████████████";
      ANSITerminal.print_string [ ANSITerminal.blue ] "████████████";
      ANSITerminal.print_string [ ANSITerminal.green ]
        "████████████████████";
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "███████████████████"
  | _ -> print_endline ""

let print_house (num : int) =
  match num with
  | 0 -> ANSITerminal.print_string [ ANSITerminal.yellow ] "       _"
  | 1 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "     _|=|__________"
  | 2 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "    /              \\"
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "   /                \\"
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "  /__________________\\"
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "   ||  || /--\\ ||  ||"
  | 6 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "   ||[]|| | .| ||[]||"
  | 7 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        " ()||__||_|__|_||__||()"
  | 8 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "( )|-|-|-|====|-|-|-|( ) "
  | 9 ->
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "^^^^^^^^^^====^^^^^^^^^^^"
  | _ -> print_endline ""

let print_lawsuit (num : int) =
  match num with
  | 0 -> ANSITerminal.print_string [ ANSITerminal.white ] "(\\"
  | 1 -> ANSITerminal.print_string [ ANSITerminal.white ] "\\'\\"
  | 2 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        " \\'\\     __________"
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        " / '|   ()_________)"
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        " \\ '/    \\ ~~~~~~~~ \\"
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "   \\       \\~~~~~~   \\"
  | 6 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "   ==).      \\__________\\"
  | 7 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "  (__)       ()__________)"
  | _ -> print_endline ""

let print_children (num : int) =
  match num with
  | 0 -> ANSITerminal.print_string [ ANSITerminal.white ] "   ,=\"\"=,"
  | 1 -> ANSITerminal.print_string [ ANSITerminal.white ] "  c , _,{"
  | 2 -> ANSITerminal.print_string [ ANSITerminal.white ] "  /\\  @ )"
  | 3 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        " /  ^~~^\\          <=.,__/ '}="
  | 4 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "(_/ ,, ,,)          \\_ _>_/~"
  | 5 ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        " ~\\_(/-\\)'-,_,_,_,-'(_)-(_)"
  | _ -> print_endline ""
