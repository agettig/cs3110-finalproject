<<<<<<< Updated upstream
open Cards
open Players

let rec find_career_card (deck : cards list) =
=======
let rec find_career_card (deck : Types.cards list) =
>>>>>>> Stashed changes
  match deck with
  | [] -> failwith "No career found"
  | h :: t -> (
      match h with
      | Career c -> (c.salary, c.salary_max, c.taxes_due)
      | _ -> find_career_card t)

let nth_of_career_tuple (career : int * int * int) (num : int) =
  match career with
  | a, b, c ->
      if num = 0 then a
      else if num = 1 then b
      else if num = 2 then c
      else failwith "out of bounds index"

let cap_at_salary_max (original : int) (salary_max : int) =
  if original > salary_max then salary_max else original

let calculate_payday (card_info : int * int * int) (pay_raise : int) =
  cap_at_salary_max
    (nth_of_career_tuple card_info 0 + pay_raise)
    (nth_of_career_tuple card_info 1)

<<<<<<< Updated upstream
let loan (player : player) : player =
=======
let loan (player : Types.player) : Types.player =
>>>>>>> Stashed changes
  {
    player with
    account_balance = player.account_balance + 20000;
    debt = player.debt + 25000;
  }

<<<<<<< Updated upstream
let rec add_houses_and_life_tiles (deck : cards list) (acc : int) =
=======
let rec add_houses_and_life_tiles (deck : Types.cards list) (acc : int)
    =
>>>>>>> Stashed changes
  match deck with
  | [] -> acc
  | h :: t -> (
      match h with
      | House ho -> add_houses_and_life_tiles t (acc + ho.selling_price)
      | Life_Tiles lt -> add_houses_and_life_tiles t (acc + lt)
      | _ -> add_houses_and_life_tiles t acc)
(* ---------- helper functions end ----------*)

(** [add_balance player amount] returns a [player] with [amount] added
    to their current balance *)
<<<<<<< Updated upstream
let add_balance (player : player) (amount : int) : player =
=======
let add_balance (player : Types.player) (amount : int) : Types.player =
>>>>>>> Stashed changes
  { player with account_balance = player.account_balance + amount }

(** [payraise player] returns a [player] with $10,000 added to their
    current pay_raise *)
<<<<<<< Updated upstream
let payraise (player : player) : player =
=======
let payraise (player : Types.player) : Types.player =
>>>>>>> Stashed changes
  { player with pay_raise = player.pay_raise + 10000 }

(** [calculate_loans player] returns a [player] with continous loans
    (loans are 20,000 added to their current balance and 25,000 added to
    their current debt)added until their account balance is positive *)
<<<<<<< Updated upstream
let rec calculate_loans (player : player) =
=======
let rec calculate_loans (player : Types.player) =
>>>>>>> Stashed changes
  if player.account_balance >= 0 then player
  else calculate_loans (loan player)

(** [pay_college player] returns a [player] with 50,000 added to their
    current debt *)
<<<<<<< Updated upstream
let pay_college (player : player) : player =
=======
let pay_college (player : Types.player) : Types.player =
>>>>>>> Stashed changes
  { player with debt = player.debt + 50000 }

(** [payday player] returns a [player] with their current career's pay
    and the player's pay_raise (pay + pay_raise is limited at their
    career's salary max) added to their current balance *)
<<<<<<< Updated upstream
let payday (player : player) : player =
=======
let payday (player : Types.player) : Types.player =
>>>>>>> Stashed changes
  {
    player with
    account_balance =
      player.account_balance
      + calculate_payday (find_career_card player.deck) player.pay_raise;
  }

(** [tax player] returns a [player] with their career's taxes subtracted
    from their current balance *)
<<<<<<< Updated upstream
let tax (player : player) : player =
=======
let tax (player : Types.player) : Types.player =
>>>>>>> Stashed changes
  calculate_loans
    {
      player with
      account_balance =
        player.account_balance
        - nth_of_career_tuple (find_career_card player.deck) 2;
    }

(** [final_balance player] returns the final balance of the [player]
    which includes taking into account their current balance, debt,
    house, and life tiles *)
<<<<<<< Updated upstream
let final_balance (player : player) =
=======
let final_balance (player : Types.player) =
>>>>>>> Stashed changes
  player.account_balance - player.debt
  + add_houses_and_life_tiles player.deck 0
