open Players

val add_balance : player -> int -> player
(** [add_balance player amount] returns a [player] with [amount] added
    to their current balance *)

val final_balance : player -> int
(** [final_balance player] returns the final balance of the [player]
    which includes taking into account their current balance, debt,
    house, and life tiles *)

val payraise : player -> player
(** [payraise player] returns a [player] with $10,000 added to their
    current pay_raise *)

val payday : player -> player
(** [payday player] returns a [player] with their current career's pay
    and the player's pay_raise (pay + pay_raise is limited at their
    career's salary max) added to their current balance *)

val tax : player -> player
(** [tax player] returns a [player] with their career's taxes subtracted
    from their current balance *)

val pay_college : player -> player
(** [pay_college player] returns a [player] with 100,000 added to their
    current debt *)

val calculate_loans : player -> player
(** [calculate_loans player] returns a [player] with continous loans
    (loans are 20,000 added to their current balance and 25,000 added to
    their current debt) added until their account balance is positive *)
