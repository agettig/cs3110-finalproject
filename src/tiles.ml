open Players
open Cards

type tiles =
  | PayTile of {
      name : string;
      account_change : int;
      index_tile : int;
    }
  | TaxesTile of {
      name : string;
      account_change : int;
      index_tile : int;
    }
  | LifeTile of {
      name : string;
      index_tile : int;
    }
  | CareerTile of {
      name : string;
      index_tile : int;
    }
  | FamilyTile of {
      name : string;
      player : player;
      account_change : int;
      (* account_change : int; *)
      index_tile : int;
    }
  | HouseTile of {
      name : string;
      house_chosen : Cards.cards;
      index_tile : int;
    }
  | TakeTile of {
      name : string;
      index_tile : int;
    }
  | ActionTile of {
      name : string;
      position_change : int;
      index_tile : int;
    }
  | LawsuitTile of {
      name : string;
      player_sued : Players.player;
      index_tile : int;
    }
  | SpinToWinTile of {
      name : string;
      index_tile : int;
    }

(* initializing all of the pay/collect tiles *)
let gold_tiles =
  [
    PayTile
      {
        name = "START COLLEGE!";
        account_change = -100000;
        index_tile = 1;
      };
    PayTile
      { name = "Scholarship"; account_change = 20000; index_tile = 2 };
    PayTile
      {
        name = "Buy books and supplies";
        account_change = -5000;
        index_tile = 3;
      };
    LifeTile { name = "Make new friends for life!"; index_tile = 4 };
    PayTile
      { name = "Part Time Job"; account_change = 10000; index_tile = 5 };
    LifeTile { name = "Semester In London"; index_tile = 6 };
    PayTile
      {
        name = "Spring Break in Florida";
        account_change = -5000;
        index_tile = 7;
      };
    LifeTile { name = "Honor Roll"; index_tile = 8 };
    LifeTile { name = "Graduation"; index_tile = 9 };
    CareerTile { name = "Choose Career"; index_tile = 10 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 11 };
    CareerTile { name = "START CAREER"; index_tile = 12 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 13 };
    PayTile
      {
        name = "Rent Apartment";
        account_change = 5000;
        index_tile = 14;
      };
    PayTile
      { name = "Inheritance"; account_change = 20000; index_tile = 15 };
    LifeTile
      { name = "Adopt a Pet from Animal Shelter"; index_tile = 16 };
    TakeTile { name = "Take a Share the Wealth card"; index_tile = 17 };
    LifeTile { name = "Get Engaged!"; index_tile = 18 };
    PayTile
      {
        name = "Snowboarding Accident";
        account_change = -5000;
        index_tile = 19;
      };
    ActionTile
      { name = "Elope!"; index_tile = 20; position_change = 15 };
    LifeTile { name = "Volunteer at Soup Kitchen"; index_tile = 21 };
    LifeTile { name = "Engagement Party!"; index_tile = 22 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 23 };
    PayTile
      { name = "Win a Race"; account_change = 10000; index_tile = 24 };
    LifeTile { name = "Get Married"; index_tile = 25 };
    LifeTile { name = "Engagement Party!"; index_tile = 26 };
    PayTile
      {
        name = "Wedding Reception";
        account_change = -20000;
        index_tile = 27;
      };
    LifeTile { name = "Happy Honeymoon!"; index_tile = 28 };
    TakeTile { name = "Take a Share the Wealth card"; index_tile = 29 };
    PayTile
      {
        name = "Car Accident";
        account_change = -10000;
        index_tile = 30;
      };
    PayTile
      {
        name = "Job Relocation";
        account_change = -20000;
        index_tile = 31;
      };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 32 };
    HouseTile
      {
        name = "Buy a Starter Home";
        (* need to figure out what to put here *)
        house_chosen = condo;
        index_tile = 33;
      };
    PayTile
      { name = "Win Lottery"; account_change = 50000; index_tile = 34 };
    TakeTile
      { name = "Lose your job: Take a Career Card"; index_tile = 35 };
    PayTile
      {
        name = "Buy flat-screen plasma TV";
        account_change = -5000;
        index_tile = 36;
      };
    LifeTile { name = "Baby Boy!"; index_tile = 37 };
    PayTile
      {
        name = "Furnish baby's room";
        account_change = -5000;
        index_tile = 38;
      };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 39 };
    LifeTile { name = "Baby Girl!"; index_tile = 40 };
    LifeTile { name = "Vote"; index_tile = 41 };
    PayTile
      {
        name = "Win the Ultimate Idol TV Show!";
        account_change = 100000;
        index_tile = 42;
      };
    LifeTile { name = "Twins!"; index_tile = 43 };
    PayTile
      {
        name = "Get best seats at the big game!";
        account_change = -20000;
        index_tile = 44;
      };
    LifeTile
      { name = "Attend Hollywood Movie Premiere!"; index_tile = 45 };
    LifeTile { name = "Baby Girl!"; index_tile = 46 };
    TakeTile { name = "Take a Share the Wealth card"; index_tile = 47 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 48 };
    LifeTile { name = "Learn sign language!"; index_tile = 49 };
    (* LawsuitTile {name = "Lawsuit!"; player_sued = player.name;
       index_tile = 50}; *)
    SpinToWinTile { name = "Spin to Win!"; index_tile = 51 };
    TakeTile
      { name = "Lose your job: Take a Career Card"; index_tile = 52 };
    ActionTile
      {
        name = "Continue On The Path to Life!";
        index_tile = 53;
        position_change = 54;
      };
    PayTile
      {
        name = "House Flooded";
        account_change = -40000;
        index_tile = 55;
      };
    LifeTile { name = "In-Laws Visit!"; index_tile = 56 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 57 };
    LifeTile { name = "Coach children's sport team"; index_tile = 58 };
    LifeTile { name = "Adopt Twins!"; index_tile = 59 };
    TakeTile
      { name = "Lose your job: Take a Career Card"; index_tile = 60 };
    TaxesTile
      { name = "Taxes"; account_change = -10000; index_tile = 61 };
    (* LawsuitTile {name = "Lawsuit!"; player_sued = player.name;
       index_tile = 62}; *)
    LifeTile { name = "Run for Congress!"; index_tile = 63 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 64 };
    LifeTile { name = "Baby Boy!"; index_tile = 65 };
    (* FamilyTile { name = "Take Family Cruise Vacation!"; player =
       account_change = 5000 * player.children; index_tile = 66; }; *)
    TakeTile { name = "Take a Share the Wealth card"; index_tile = 67 };
    PayTile
      {
        name = "Win On TV Game Show!";
        account_change = 100000;
        index_tile = 68;
      };
    (* LawsuitTile {name = "Lawsuit!"; player_sued = player.name;
       index_tile = 69}; *)
    LifeTile { name = "Run for Congress!"; index_tile = 63 };
    PayTile
      { name = "Art Auction"; account_change = -20000; index_tile = 70 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 71 };
    LifeTile { name = "Visit Grand Canyon!"; index_tile = 72 };
    TaxesTile
      { name = "Taxes"; account_change = -10000; index_tile = 73 };
    (* (need to add child multplier) *)
    PayTile
      {
        name = "Sports Camp for the Kids";
        account_change = -5000;
        index_tile = 74;
      };
    PayTile
      {
        name = "Donate to African Orphans";
        account_change = -40000;
        index_tile = 75;
      };
    (* LawsuitTile {name = "Lawsuit!"; player_sued = player.name;
       index_tile = 76}; *)
    LifeTile { name = "Run for Congress!"; index_tile = 63 };
    SpinToWinTile { name = "Spin to Win!"; index_tile = 77 };
    PayTile
      { name = "Buy an SUV"; account_change = -40000; index_tile = 78 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 79 };
    TakeTile
      { name = "Lose your job: Take a Career Card"; index_tile = 80 };
    TakeTile { name = "Take a Share the Wealth card"; index_tile = 81 };
    PayTile
      {
        name = "TV Dance Show Winner!";
        account_change = 100000;
        index_tile = 82;
      };
    (* LawsuitTile {name = "Lawsuit!"; player_sued = player.name;
       index_tile = 83}; *)
    LifeTile { name = "Run for Congress!"; index_tile = 63 };
    (* (need to add child multplier) *)
    PayTile
      {
        name = "Summer School!";
        account_change = -5000;
        index_tile = 84;
      };
    PayTile
      {
        name = "Buy home gym";
        account_change = -30000;
        index_tile = 85;
      };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 86 };
    LifeTile { name = "Learn CPR"; index_tile = 87 };
    PayTile
      {
        name = "Buy foreign sports car";
        account_change = -30000;
        index_tile = 88;
      };
    SpinToWinTile { name = "Spin to Win!"; index_tile = 89 };
    PayTile
      {
        name = "Find Buried Treasure";
        account_change = 500000;
        index_tile = 90;
      };
    LifeTile { name = "Baby Boy!"; index_tile = 91 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 92 };
    PayTile
      {
        name = "Buy lakeside cabin";
        account_change = 120000;
        index_tile = 93;
      };
    (* LawsuitTile {name = "Lawsuit!"; player_sued = player.name;
       index_tile = 94}; *)
    LifeTile { name = "Run for Congress!"; index_tile = 63 };
    LifeTile
      { name = "Adopt Pet from Animal Shelter!"; index_tile = 95 };
    PayTile
      {
        name = "Win Noble Prize!!";
        account_change = 100000;
        index_tile = 96;
      };
    (* (need to figure out how to add different houses) *)
    HouseTile
      {
        name = "Choose a House!";
        house_chosen = mobile_home;
        index_tile = 97;
      };
    PayTile
      {
        name = "Tornado hits house!";
        account_change = -125000;
        index_tile = 98;
      };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 99 };
    TaxesTile
      { name = "Taxes"; account_change = -10000; index_tile = 100 };
    PayTile
      {
        name = "Write Best Selling Book";
        account_change = 200000;
        index_tile = 101;
      };
    (* figure out how to multiply child *)
    PayTile
      { name = "College"; account_change = -50000; index_tile = 102 };
    (* LawsuitTile {name = "Lawsuit!"; player_sued = player.name;
       index_tile = 103}; *)
    LifeTile { name = "Run for Congress!"; index_tile = 63 };
    PayTile
      {
        name = "Buy Sailboat!";
        account_change = -30000;
        index_tile = 104;
      };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 105 };
    TaxesTile
      { name = "Taxes"; account_change = 10000; index_tile = 106 };
    (* (need to add child multplier) *)
    PayTile
      {
        name = "Take family on theme park vacation";
        account_change = -25000 * 5000;
        index_tile = 107;
      };
    LifeTile { name = "Visit Pyramids in Egypt"; index_tile = 108 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 109 };
    LifeTile { name = "Visit Old Soldiers' Home"; index_tile = 110 };
    PayTile
      {
        name = "Redorate your Home";
        account_change = -80000;
        index_tile = 111;
      };
    LifeTile { name = "You're a Grandparent!"; index_tile = 112 };
    SpinToWinTile { name = "Spin to Win!"; index_tile = 113 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 114 };
    PayTile
      {
        name = "Sponsor Public Arts Exhibit";
        account_change = -125000;
        index_tile = 115;
      };
    LifeTile { name = "Host Family Reunion!"; index_tile = 116 };
    LifeTile { name = "You're a Grandparent!"; index_tile = 117 };
    PayTile
      {
        name = "Hire Maid + Butler Service";
        account_change = -65000;
        index_tile = 118;
      };
    LifeTile
      { name = "Go Hiking in the European Alps"; index_tile = 119 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 120 };
    SpinToWinTile { name = "Spin to Win!"; index_tile = 121 };
    LifeTile
      { name = "Visit the Great Wall in China"; index_tile = 122 };
    PayTile
      {
        name = "Have a life saving Operation";
        account_change = -50000;
        index_tile = 123;
      };
    (* LawsuitTile {name = "Lawsuit!"; player_sued = player.name;
       index_tile = 124}; *)
    LifeTile { name = "Run for Congress!"; index_tile = 63 };
    PayTile
      {
        name = "Have family website designed";
        account_change = -50000;
        index_tile = 125;
      };
    LifeTile { name = "You're a Grandparent!"; index_tile = 126 };
    PayTile
      { name = "PAYDAY!"; account_change = 10000; index_tile = 127 };
    PayTile
      {
        name = "Host Entertainment Awards Party";
        account_change = -35000;
        index_tile = 128;
      };
    (* (Figure out # of spimns here) *)
    PayTile { name = "Pension"; account_change = 10000; index_tile = 4 };
  ]