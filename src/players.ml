open Cards
open Board

type player = {
  name : string;
  children : int;
  so : bool;
  deck : cards list;
  account_balance : int;
  debt : int;
  pay_raise : int;
  college : bool;
  index_on_board : int;
  colorblind : colorblind;
}

(** [taxes_aux lst] will return an int representing the taxes due for
    the career in [lst]. Requires [lst] has a career.*)
let rec taxes_aux lst =
  match lst with
  | [] -> raise Not_found
  | h :: t -> (
      match h with
      | Career c -> c.taxes_due
      | _ -> taxes_aux t)

let get_taxes player = taxes_aux player.deck

(** [deck_string_helper deck acc] will return a string containing each
    card name and what type of card it is*)
let rec deck_string_helper (deck : cards list) (acc : string) =
  match deck with
  | [] -> acc
  | [ h ] -> (
      match h with
      | House c -> acc ^ "House: " ^ c.name
      | Career c -> acc ^ "Career: " ^ c.name
      | Long_Term_Investment c ->
          acc ^ "Long Term Investments: " ^ string_of_int c
      | Life_Tiles _ -> acc ^ "LifeTile"
      | Exemption_Card -> acc ^ "Exemption Card"
      | SpinToWin_Card x -> acc ^ "Spin to Win: " ^ string_of_int x)
  | h :: t -> (
      match h with
      | House c -> deck_string_helper t (acc ^ "House: " ^ c.name ^ ", ")
      | Career c ->
          deck_string_helper t (acc ^ "Career: " ^ c.name ^ ", ")
      | Long_Term_Investment c ->
          deck_string_helper t
            (acc ^ "Long Term Investments: " ^ string_of_int c ^ ", ")
      | Life_Tiles _ -> deck_string_helper t (acc ^ "LifeTile" ^ ", ")
      | Exemption_Card ->
          deck_string_helper t (acc ^ "Exemption Card" ^ ", ")
      | SpinToWin_Card x ->
          deck_string_helper t
            (acc ^ "Spin to Win: " ^ string_of_int x ^ ", "))

(** [add_player player_name attended_college] returns a [player] with
    initialized parameters *)
let add_player
    (player_name : string)
    (attended_college : bool)
    (colorblind_op : colorblind) =
  {
    name = player_name;
    children = 0;
    so = false;
    deck = [];
    account_balance = (if attended_college then 100000 else 10000);
    debt = (if attended_college then 100000 else 0);
    pay_raise = 0;
    college = attended_college;
    index_on_board = (if attended_college then 0 else 10);
    colorblind = colorblind_op;
  }

let add_children (player : player) (num_children : int) =
  { player with children = player.children + num_children }

let add_significant_other (player : player) = { player with so = true }

let rec remove_from_deck
    (deck : cards list)
    (card : cards)
    (acc : cards list) =
  match deck with
  | [] -> acc
  | h :: t ->
      if h = card then remove_from_deck t card acc
      else remove_from_deck t card (h :: acc)

let add_card (card : cards) (player : player) : player =
  { player with deck = card :: player.deck }

let remove_card (card : cards) (player : player) : player =
  { player with deck = remove_from_deck player.deck card [] }

let exchange_card
    (player : player)
    (card_to_add : cards)
    (card_to_remove : cards) =
  player |> add_card card_to_add |> remove_card card_to_remove

let player_to_string (player : player) =
  let () = print_endline ("Name: " ^ player.name) in
  let () =
    print_endline ("# of Children: " ^ string_of_int player.children)
  in
  let () = print_endline ("Married: " ^ string_of_bool player.so) in
  let () =
    print_endline ("Deck: " ^ deck_string_helper player.deck "")
  in
  let () =
    print_endline
      ("Account Balance: $" ^ string_of_int player.account_balance)
  in
  let () = print_endline ("Debt: $" ^ string_of_int player.debt) in
  let () =
    print_endline ("Pay Raise: $" ^ string_of_int player.pay_raise)
  in
  let () =
    print_endline ("Went to College: " ^ string_of_bool player.college)
  in
  print_endline "------------------------------------------";
  print_endline
    ("You are on tile "
    ^ string_of_int (player.index_on_board + 1)
    ^ ".");
  print_color_tile player.index_on_board make_board player.colorblind;
  print_endline ""
