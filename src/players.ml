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