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
export type CState = 'empty'|'miss'|'hit'|'ship'|'wait';
export type SeaLine = Array<CState>;
export type Sea = Array<SeaLine>;
export type Person = {
  name: string;
  message: string;
  map?: Sea;
  new?: boolean;
}
export type Game = {
  rules: string;
  turn: 'notready'|'owner'|'player'|'owner_win'|'palyer_win';
  you: 'owner'|'player'|'guest';
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
  mode: 'pre'|'init'|'create'|'join'|'loading'|'game'|'make_public';
  gameid?: string;
  session?: string;
  init?: Array<PublicGame>;
  join?: PublicGame;
  game?: Game;
  initSea?: Sea;
  currentBoard?: "my"|"enemy";
  currentPos?: Point;
  chat?: Array<Message>;
  api: string;
}
