import * as React from 'react';

export type Props = any;
export type Element = React.ReactElement<any>;
export type Component = (p:Props) => Element;
export type Box = {
  contramap: (f:(p:Props) => Props) => Box;
  concat: (b:Box) => Box;
  fold: Component;
  map: (f:(e:Element) => Element) => Box;
};

export type PublicGame = {
  rules: string;
  game: string;
  owner: string;
  message: string;
  turn: string;
}
export type Rule = {
  rules: string;
  name: string;
  id: string;
  order: number;
}
export type Message = {
  name: string;
  message: string;
  time: Number;
}
export type Point = {
  x: number;
  y: number;
};
export type TabItem = {
  name: string;
  component: (g:Battleship) => React.ReactElement<any>;
};

// Map:
//   0 - empty
//   1 - ship
//   2 - miss
//   3 - hit
//   4 - wait
export type CState = 0|1|2|3|4;
export type SeaLine = Array<CState>;
export type Sea = Array<SeaLine>;
export type Person = {
  name: string;
  message: string;
  map?: Sea;
  new?: boolean;
}
export type Turn = 'notready'|'config'|'owner'|'player'|'owner_win'|'player_win';
export type Mode = 'pre'|'init'|'create'|'join'|'loading'|'game'|'make_public'|'are_you_sure';
export type You = 'owner'|'player'|'guest';
export type BoardType = "right"|"left";
export type Game = {
  rules: string;
  turn: Turn;
  you: You;
  yourname: string;
  game: string;
  message: string;
  owner: Person;
  isPublic: boolean;
  player?: Person;
  guests?: Array<Person>;
  unread?: number;
}
export type Battleship = {
  rules?: Array<Rule>;
  mode: Mode;
  gameid?: string;
  session?: string;
  init?: Array<PublicGame>;
  join?: PublicGame;
  game?: Game;
  initSea?: Sea;
  currentBoard?: BoardType;
  currentPos?: Point;
  chat?: Array<Message>;
  api: string;
  popupName?: string;
  popupMessage?: string;
  popupError?: string;
  popupRules?: string;
  bottom?: number;
  message?: string;
  chatmessage?: string;
}
