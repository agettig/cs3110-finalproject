open Cards

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
}

let rec taxes_aux lst =
  match lst with
  | [] -> raise Not_found
  | h :: t -> (
      match h with
      | Career c -> c.taxes_due
      | _ -> taxes_aux t)

(** [get_taxes player] returns the amount of taxes for [player] based on
    their career. Requires [Player] has a career.*)
let get_taxes player = taxes_aux player.deck

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
      | _ -> acc)
  | h :: t -> (
      match h with
      | House c -> deck_string_helper t (acc ^ "House: " ^ c.name ^ ", ")
      | Career c ->
          deck_string_helper t (acc ^ "Career: " ^ c.name ^ ", ")
      | Long_Term_Investment c ->
          deck_string_helper t
            (acc ^ "Long Term Investments: " ^ string_of_int c ^ ", ")
      | Life_Tiles _ -> deck_string_helper t (acc ^ "LifeTile" ^ ", ")
      | _ -> deck_string_helper t acc)

(** [add_player player_name attended_college] returns a [player] with
    initialized parameters *)
let add_player (player_name : string) (attended_college : bool) =
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
  }

(** [add_children player num_children] returns a [player] with
    [num_children] added to their children count *)
let add_children (player : player) (num_children : int) =
  { player with children = player.children + num_children }

(** [add_significant_other player] returns a [player] with significant
    other set to true *)
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

(** [add_card player card] returns a [player] with [card] added to their
    current deck *)
let add_card (card : cards) (player : player) : player =
  { player with deck = card :: player.deck }

(** [remove_card player card] returns a [player] with [card] removed
    from their current deck *)
let remove_card (card : cards) (player : player) : player =
  { player with deck = remove_from_deck player.deck card [] }

(** [exchange_card player card_to_add card_to_remove] returns a [player]
    with [card_to_add] added to their current deck and [card_to_remove]
    removed from their current deck *)
let exchange_card
    (player : player)
    (card_to_add : cards)
    (card_to_remove : cards) =
  player |> add_card card_to_add |> remove_card card_to_remove

(** [player_to_string player] returns a string containing all info of
    the player including name, deck, balance, debt, payraise, college,
    and index on board *)
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
  print_endline
    ("Index on board: " ^ string_of_int player.index_on_board)
