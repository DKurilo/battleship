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
  rules: String;
  game: String;
  owner: String;
  message: String;
}
export type Rule = {
  rules: String;
  name: String;
  id: String;
  order: number;
}
export type Message = {
  name: String;
  message: String;
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
  name: String;
  message: String;
  map?: Sea;
  new: boolean;
}
export type Game = {
  rules: String;
  turn: 'notready'|'owner'|'player'|'owner_win'|'palyer_win';
  you: 'owner'|'player'|'guest';
  game: String;
  message: String;
  owner: Person;
  player?: Person;
  guests?: Array<Person>;
  unread?: number;
}
export type Battleship = {
  rules?: Array<Rule>;
  mode: 'pre'|'init'|'create'|'join'|'loading'|'game'|'make_public';
  gameid?: String;
  session?: String;
  pre?: Array<PublicGame>;
  game?: Game;
  initSea?: Sea;
  currentBoard?: "my"|"enemy";
  currentPos?: Point;
  chat?: Array<Message>
}
