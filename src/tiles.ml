open Bank
open Cards

type tiles =
  | PayTile of {
      name : string;
      account_change : int;
      index_tile : int;
    }
  | DebtTile of {
      name : string;
      account_change : int;
      index_tile : int;
    }
  | TaxesTile of {
      name : string;
      account_change : int;
      loans : int;
      player_sued : Types.player;
      index_tile : int;
    }
  | LifeTile
  | CareerTile
  | FamilyTile
  | HouseTile

(* initializing all of the pay/collect tiles *)
let gold_tiles =
  [
    PayTile
      {
        name = "Hire Maid + Butler Service";
        account_change = -65000;
        index_tile = 0;
      };
    PayTile
      {
        name = "Have a life saving Operation";
        account_change = -50000;
        index_tile = 1;
      };
    PayTile
      {
        name = "Have family website designed";
        account_change = -50000;
        index_tile = 2;
      };
    PayTile
      {
        name = "Host Awards Party";
        account_change = -35000;
        index_tile = 3;
      };
    PayTile { name = "Pension"; account_change = 10000; index_tile = 4 };
    PayTile
      {
        name = "Sponsor Public Arts Exhibit";
        account_change = -125000;
        index_tile = 5;
      };
    PayTile
      {
        name = "Redorate your Home";
        account_change = -80000;
        index_tile = 6;
      };
    PayTile
      {
        name = "Take family on theme park vacation";
        account_change = 25000 + 5000;
        index_tile = 7;
      };
    PayTile
      {
        name = "Buy Sailboat!";
        account_change = -30000;
        index_tile = 8;
      };
    PayTile
      { name = "College"; account_change = -50000; index_tile = 9 };
    PayTile
      {
        name = "Write Best Selling Book";
        account_change = 200000;
        index_tile = 10;
      };
    PayTile
      {
        name = "Tornado hits house!";
        account_change = -125000;
        index_tile = 11;
      };
    PayTile
      {
        name = "Win Noble Prize!!";
        account_change = 100000;
        index_tile = 12;
      };
    PayTile
      {
        name = "Buy lakeside cabin";
        account_change = 120000;
        index_tile = 13;
      };
    PayTile
      {
        name = "Find Buried Treasure";
        account_change = 500000;
        index_tile = 14;
      };
    PayTile
      {
        name = "Buy foreign sports car";
        account_change = -30000;
        index_tile = 15;
      };
    PayTile
      {
        name = "Buy home gym";
        account_change = -30000;
        index_tile = 16;
      };
    (* PayTile { name = "Taxes Due!"; account_change = account_balance +
       cards.taxes_due); index_tile = 17 }; *)
  ]

(* let blue_tiles = [ TaxesTile { name = "LAWSUIT" } ] *)
