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


