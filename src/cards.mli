open Yojson.Basic.Util

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
(** [remove_first instance c deck acc] returns the deck with the first
    instance of c removed. Returns deck if c is not in deck*)

val extract_house : Yojson.Basic.t -> cards
(** [extract_house json] creates a House card with given data from
    [json] *)

val extract_career : Yojson.Basic.t -> cards
(** [extract_career json] creates a Career card with given data from
    [json] *)

val extract_spin_to_win : Yojson.Basic.t -> cards
(** [extract_spin_to_win json] creates a Spin to Win card with given
    data from [json] *)

val extract_long_term_investment : Yojson.Basic.t -> cards
(** [extract_long_term_investment json] creates a Long Term Investment
    card with given data from [json] *)

val extract_life_tile : Yojson.Basic.t -> cards
(** [extract_life_tile json] creates a Life Tile card with given data
    from [json] *)

val extract_house_lst : Yojson.Basic.t -> cards list
(** [extract_house_lst json] creates a list of House cards with given
    data from [json] *)

val extract_careers_lst : Yojson.Basic.t -> cards list
(** [extract_careers_lst json] creates a list of Career cards with given
    data from [json] *)

val extract_life_tiles_lst : Yojson.Basic.t -> cards list
(** [extract_life_tiles_lst json] creates a list of Life Tile cards with
    given data from [json] *)

val extract_spin_to_win_cards_lst : Yojson.Basic.t -> cards list
(** [extract_hspin_to_win_lst json] creates a list of Spin to Win cards
    with given data from [json] *)

val extract_long_term_investment_cards_lst :
  Yojson.Basic.t -> cards list
(** [extract_long_term_investment_cards_lst json] creates a list of Long
    Term Investment cards with given data from [json] *)

val card_tree : Yojson.Basic.t
(** [card_tree] is the json file that contains the data for the cards in
    the Game of Life *)
