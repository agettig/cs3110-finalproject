open Players

type gamestate = {
  current_player : player;
  players : player * int list;
}