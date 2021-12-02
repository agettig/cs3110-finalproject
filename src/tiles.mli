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

val gold_tiles : tiles list
(** [gold_tiles] are tiles of the board in The Game of Life *)

val print_tiles : tiles -> unit
(** [print_tiles t] prints [t]'s name and important information *)

module IntHashtbl : sig
  type 'a t

  val find : 'a t -> int -> 'a
end

module IntHash : sig
  type t = int

  val equal : t -> t -> bool

  val hash : t -> t
end

val int_tile : tiles IntHashtbl.t