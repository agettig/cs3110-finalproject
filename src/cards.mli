type cards =
  | House of {
      name : string;
      price : int;
      selling_price : int;
      starter : bool;
    }
  | Career of {
      name : string;
      salary : int;
      salary_max : int;
      taxes_due : int;
      college_career : bool;
    }
  | Exemption_Card
  | SpinToWin_Card of int
  | Long_Term_Investment of int
  | Life_Tiles of int

val careers : cards list
(** [careers] is a list of careers in the game *)

val lg_tm_invt : cards list
(** [lg_tm_invt] is a list of long term investment cards in the game *)

val life_tiles : cards list
(** [life_tiles] is a list of life_tiles in the game *)

val exemption_card_lst : cards list
(** [exemption_card_lst] is a list of exemption cards in the game *)

val spin_to_win_lst : cards list
(** [spin_to_win_lst] is a list of spin to win cards in the game *)

val share_wealth_cards : cards list
(** [share_wealth_cards] is a list of share the wealth cards in the game
    (consists of exemption cards and spin to win cards) *)

val houses : cards list
(** [houses] is a list of house cards in the game *)

val remove_first_instance :
  cards -> cards list -> cards list -> cards list
(** [remove_first)instance c deck acc] returns the deck with the first
    instance of c removed. Returns deck if c is not in deck*)
