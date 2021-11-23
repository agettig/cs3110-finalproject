open Cards
open Players

(** [find_career_card deck] will return the career card found in [deck].
    Requires [deck] has a career.*)
let rec find_career_card (deck : cards list) =
  match deck with
  | [] -> failwith "No career found"
  | h :: t -> (
      match h with
      | Career c -> (c.salary, c.salary_max, c.taxes_due)
      | _ -> find_career_card t)

(** [nth_of_career_tuple career num] returns the zeroth, first, or
    second element of [career] based on the [num] passed in*)
let nth_of_career_tuple (career : int * int * int) (num : int) =
  match career with
  | a, b, c ->
      if num = 0 then a
      else if num = 1 then b
      else if num = 2 then c
      else failwith "out of bounds index"

(** [cap_at_salary_max original salary_max] returns [original] unless it
    exceeds [salary_max], in which it will return [salary_max]*)
let cap_at_salary_max (original : int) (salary_max : int) =
  if original > salary_max then salary_max else original

(** [calculate_payday card_info pay_raise] returns an int representing
    the money rewarded from the payday*)
let calculate_payday (card_info : int * int * int) (pay_raise : int) =
  cap_at_salary_max
    (nth_of_career_tuple card_info 0 + pay_raise)
    (nth_of_career_tuple card_info 1)

(** [loan player] returns a [player] with 20,000 added to their account
    balance and 25,000 added to thier debt*)
let loan (player : player) : player =
  {
    player with
    account_balance = player.account_balance + 20000;
    debt = player.debt + 25000;
  }

(** [add_houses_and_life_tiles deck acc] returns an [acc] that is the
    sum of selling prices of all house tiles and values of all life
    tiles *)
let rec add_houses_and_life_tiles (deck : cards list) (acc : int) =
  match deck with
  | [] -> acc
  | h :: t -> (
      match h with
      | House ho -> add_houses_and_life_tiles t (acc + ho.selling_price)
      | Life_Tiles lt -> add_houses_and_life_tiles t (acc + lt)
      | _ -> add_houses_and_life_tiles t acc)

(** [calculate_loans player] returns a [player] with continous loans
    (loans are 20,000 added to their current balance and 25,000 added to
    their current debt) added until their account balance is positive *)
let rec calculate_loans (player : player) =
  if player.account_balance >= 0 then player
  else calculate_loans (loan player)
(* ---------- helper functions end ----------*)

let add_balance (player : player) (amount : int) : player =
  calculate_loans
    { player with account_balance = player.account_balance + amount }

let payraise (player : player) : player =
  { player with pay_raise = player.pay_raise + 10000 }

let pay_college (player : player) : player =
  { player with debt = player.debt + 100000; college = true }

let payday (player : player) : player =
  {
    player with
    account_balance =
      player.account_balance
      + calculate_payday (find_career_card player.deck) player.pay_raise;
  }

let tax (player : player) : player =
  calculate_loans
    {
      player with
      account_balance =
        player.account_balance
        - nth_of_career_tuple (find_career_card player.deck) 2;
    }

let final_balance (player : player) =
  player.account_balance - player.debt
  + add_houses_and_life_tiles player.deck 0
