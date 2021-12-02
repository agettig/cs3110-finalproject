open Yojson.Basic.Util

type tiles =
  | PayTile of {
      name : string;
      account_change : int;
      index_tile : int;
    }
  | TaxesTile of {
      name : string;
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
      account_change : int;
      index_tile : int;
      children : int;
    }
  | HouseTile of {
      name : string;
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
      index_tile : int;
    }
  | SpinToWinTile of {
      name : string;
      index_tile : int;
    }

let extract_pay_tile json =
  let name = json |> member "name" |> to_string in
  let account_change = json |> member "account_change" |> to_int in
  let index_tile = json |> member "index_tile" |> to_int in
  PayTile { name; account_change; index_tile }

let extract_life_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  LifeTile { name; index_tile }

let extract_career_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  CareerTile { name; index_tile }

let extract_take_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  TakeTile { name; index_tile }

let extract_family_tile json =
  let name = json |> member "name" |> to_string in
  let account_change = json |> member "account_change" |> to_int in
  let index_tile = json |> member "index_tile" |> to_int in
  let children = json |> member "children" |> to_int in
  FamilyTile { name; account_change; index_tile; children }

let extract_house_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  HouseTile { name; index_tile }

let extract_lawsuit_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  LawsuitTile { name; index_tile }

let extract_spin_to_win_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  SpinToWinTile { name; index_tile }

let extract_action_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  let position_change = json |> member "position_change" |> to_int in
  ActionTile { name; index_tile; position_change }

let extract_taxes_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  TaxesTile { name; index_tile }

let print_tiles = function
  | PayTile x ->
      Printf.printf "Name: %s\nAccount Change: %i\n \n" x.name
        x.account_change
  | TaxesTile _ -> Printf.printf "Taxes Due\n \n"
  | LifeTile x -> Printf.printf "Name: %s\n \n" x.name
  | CareerTile x -> Printf.printf "Name: %s\n \n" x.name
  | FamilyTile x ->
      Printf.printf "Name: %s\nAccount Change: %i\nChildren: %i\n \n"
        x.name x.account_change x.children
  | HouseTile x -> Printf.printf "Name: %s\n \n" x.name
  | TakeTile x -> Printf.printf "Name: %s\n \n" x.name
  | ActionTile x ->
      Printf.printf "Name: %s\nGo here: %i\n \n" x.name
        x.position_change
  | LawsuitTile x -> Printf.printf "Name: %s\n \n" x.name
  | SpinToWinTile x -> Printf.printf "Name: %s\n \n" x.name
