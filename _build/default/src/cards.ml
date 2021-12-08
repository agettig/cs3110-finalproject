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

let rec remove_first_instance
    (card : cards)
    (card_list : cards list)
    (acc : cards list) =
  match card_list with
  | [] -> acc
  | h :: t ->
      if h = card then acc @ t
      else remove_first_instance card t (h :: acc)

let extract_house json =
  let name = json |> member "name" |> to_string in
  let price = json |> member "price" |> to_int in
  let selling_price = json |> member "selling price" |> to_int in
  let starter = json |> member "starter" |> to_bool in
  House { name; price; selling_price; starter }

let extract_career json =
  let name = json |> member "name" |> to_string in
  let salary = json |> member "salary" |> to_int in
  let salary_max = json |> member "salary max" |> to_int in
  let taxes_due = json |> member "taxes due" |> to_int in
  let college_career = json |> member "college career" |> to_bool in
  Career { name; salary; salary_max; taxes_due; college_career }

let extract_spin_to_win json =
  let number = json |> member "number" |> to_int in
  SpinToWin_Card number

let extract_long_term_investment json =
  let number = json |> member "spin" |> to_int in
  Long_Term_Investment number

let extract_life_tile json =
  let number = json |> member "value" |> to_int in
  Life_Tiles number

let extract_house_lst json =
  json |> member "house cards" |> to_list |> List.map extract_house

let extract_careers_lst json =
  json |> member "career cards" |> to_list |> List.map extract_career

let extract_life_tiles_lst json =
  json
  |> member "life tile cards"
  |> to_list
  |> List.map extract_life_tile

let extract_spin_to_win_cards_lst json =
  json
  |> member "spin to win cards"
  |> to_list
  |> List.map extract_spin_to_win

let extract_long_term_investment_cards_lst json =
  json
  |> member "long term investment cards"
  |> to_list
  |> List.map extract_long_term_investment

let card_tree = Yojson.Basic.from_file "data/cards.json"

let exemption_card_lst =
  [
    Exemption_Card;
    Exemption_Card;
    Exemption_Card;
    Exemption_Card;
    Exemption_Card;
    Exemption_Card;
    Exemption_Card;
    Exemption_Card;
  ]

let houses = extract_house_lst card_tree

let careers = extract_careers_lst card_tree

let spin_to_win_lst = extract_spin_to_win_cards_lst card_tree

let lg_tm_invt = extract_long_term_investment_cards_lst card_tree

let life_tiles = extract_life_tiles_lst card_tree

(* let rec deck_string_helper (deck : cards list) (acc : string) = match
   deck with | [] -> acc | [ h ] -> ( match h with | House c -> acc ^
   "House: " ^ c.name | Career c -> acc ^ "Career: " ^ c.name |
   Long_Term_Investment c -> acc ^ "Long Term Investments: " ^
   string_of_int c | Life_Tiles _ -> acc ^ "LifeTile " | Exemption_Card
   -> acc ^ "Exemption Card" | SpinToWin_Card x -> acc ^ "Spin to Win: "
   ^ string_of_int x | _ -> acc) | h :: t -> ( match h with | House c ->
   deck_string_helper t (acc ^ "House: " ^ c.name ^ ", ") | Career c ->
   deck_string_helper t (acc ^ "Career: " ^ c.name ^ ", ") |
   Long_Term_Investment c -> deck_string_helper t (acc ^ "Long Term
   Investments: " ^ string_of_int c ^ ", ") | Life_Tiles _ ->
   deck_string_helper t (acc ^ "LifeTile" ^ ", ") | Exemption_Card ->
   deck_string_helper t (acc ^ "Exemption Card" ^ ", ") | SpinToWin_Card
   x -> deck_string_helper t (acc ^ "Spin to Win: " ^ string_of_int x ^
   ", ") | _ -> deck_string_helper t acc) *)

let share_wealth_cards = exemption_card_lst @ spin_to_win_lst

let deck = life_tiles @ careers @ houses @ share_wealth_cards

(* let str = print_endline (Players.deck_string_helper deck "") *)