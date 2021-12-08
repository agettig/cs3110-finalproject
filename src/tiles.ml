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

let extract_pay_tile_list json =
  json |> member "pay tile" |> to_list |> List.map extract_pay_tile

let extract_life_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  LifeTile { name; index_tile }

let extract_life_tile_list json =
  json |> member "life tile" |> to_list |> List.map extract_life_tile

let extract_career_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  CareerTile { name; index_tile }

let extract_career_tile_list json =
  json |> member "career tile" |> to_list
  |> List.map extract_career_tile

let extract_take_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  TakeTile { name; index_tile }

let extract_take_tile_list json =
  json |> member "take tile" |> to_list |> List.map extract_take_tile

let extract_family_tile json =
  let name = json |> member "name" |> to_string in
  let account_change = json |> member "account_change" |> to_int in
  let index_tile = json |> member "index_tile" |> to_int in
  let children = json |> member "children" |> to_int in
  FamilyTile { name; account_change; index_tile; children }

let extract_family_tile_list json =
  json |> member "family tile" |> to_list
  |> List.map extract_family_tile

let extract_house_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  HouseTile { name; index_tile }

let extract_house_tile_list json =
  json |> member "house tile" |> to_list |> List.map extract_house_tile

let extract_lawsuit_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  LawsuitTile { name; index_tile }

let extract_lawsuit_tile_list json =
  json |> member "lawsuit tile" |> to_list
  |> List.map extract_lawsuit_tile

let extract_spin_to_win_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  SpinToWinTile { name; index_tile }

let extract_spin_to_win_tile_list json =
  json |> member "spintowin tile" |> to_list
  |> List.map extract_spin_to_win_tile

let extract_action_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  let position_change = json |> member "position_change" |> to_int in
  ActionTile { name; index_tile; position_change }

let extract_action_tile_list json =
  json |> member "action tile" |> to_list
  |> List.map extract_action_tile

let extract_taxes_tile json =
  let name = json |> member "name" |> to_string in
  let index_tile = json |> member "index_tile" |> to_int in
  TaxesTile { name; index_tile }

let extract_taxes_tile_list json =
  json |> member "taxes tile" |> to_list |> List.map extract_taxes_tile

let tiles = Yojson.Basic.from_file "data/tiles.json"

let pay_tile = extract_pay_tile_list tiles

let life_tile = extract_life_tile_list tiles

let career_tile = extract_career_tile_list tiles

let take_tile = extract_take_tile_list tiles

let family_tile = extract_family_tile_list tiles

let house_tile = extract_house_tile_list tiles

let lawsuit_tile = extract_lawsuit_tile_list tiles

let spintowin_tile = extract_spin_to_win_tile_list tiles

let action_tile = extract_action_tile_list tiles

let taxes_tile = extract_taxes_tile_list tiles

let gold_tiles =
  pay_tile @ life_tile @ career_tile @ take_tile @ family_tile
  @ house_tile @ lawsuit_tile @ spintowin_tile @ action_tile
  @ taxes_tile

module IntHash = struct
  type t = int

  let equal i j = i = j

  let hash i = i land max_int
end

module IntHashtbl = Hashtbl.Make (IntHash)

let get_index_of_tile = function
  | PayTile x -> x.index_tile
  | TaxesTile x -> x.index_tile
  | LifeTile x -> x.index_tile
  | CareerTile x -> x.index_tile
  | FamilyTile x -> x.index_tile
  | HouseTile x -> x.index_tile
  | TakeTile x -> x.index_tile
  | ActionTile x -> x.index_tile
  | LawsuitTile x -> x.index_tile
  | SpinToWinTile x -> x.index_tile

let hashtable = IntHashtbl.create 100

let rec tiles (tiles_lst : tiles list) (hash : tiles IntHashtbl.t) :
    tiles IntHashtbl.t =
  match tiles_lst with
  | [] -> hash
  | h :: t ->
      IntHashtbl.add hash (get_index_of_tile h) h;
      tiles t hash

let int_tile = tiles gold_tiles hashtable

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

(* let printer_tiles = IntHashtbl.iter (fun index tile -> print_endline
   (string_of_int index); print_tiles tile) int_tile *)
