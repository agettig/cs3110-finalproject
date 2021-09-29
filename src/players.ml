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
}

let add_player
    (player_name : string)
    (player_deck : cards list)
    (player_acct_balance : int)
    (player_debt : int)
    (player_pay_raise : int)
    (attended_college : bool) =
  {
    name = player_name;
    children = 0;
    so = false;
    deck = player_deck;
    account_balance = player_acct_balance;
    debt = player_debt;
    pay_raise = player_pay_raise;
    college = attended_college;
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
