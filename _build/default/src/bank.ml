(** [add_balance player amount] returns a player with amount added to their current balance *)
    let add_balance (player : Types.player) (amount : int) : Types.player = 
      {player with account_balance = player.account_balance + amount}