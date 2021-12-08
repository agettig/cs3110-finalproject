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

val get_taxes : player -> int
(** [get_taxes player] returns the amount of taxes for [player] based on
    their career. Requires [Player] has a career.*)

val add_player : string -> bool -> colorblind -> player
(** [add_player player_name attended_college colorblind] returns a
    [player] with initialized parameters *)

val add_children : player -> int -> player
(** [add_children player num_children] returns a [player] with
    [num_children] added to their children count *)

val add_significant_other : player -> player
(** [add_significant_other player] returns a [player] with significant
    other set to true *)

val remove_from_deck : cards list -> cards -> cards list -> cards list
(** [remove_from_deck d c acc] returns [acc] with cards from [d] except
    for [c] *)

val add_card : cards -> player -> player
(** [add_card player card] returns a [player] with [card] added to their
    current deck *)

val remove_card : cards -> player -> player
(** [remove_card player card] returns a [player] with [card] removed
    from their current deck *)

val exchange_card : player -> cards -> cards -> player
(** [exchange_card player card_to_add card_to_remove] returns a [player]
    with [card_to_add] added to their current deck and [card_to_remove]
    removed from their current deck *)

val player_to_string : player -> unit
(** [player_to_string player] returns a string containing all info of
    the player including name, deck, balance, debt, payraise, college,
    and index on board *)
