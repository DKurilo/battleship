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

export type Point = {
  x: number;
  y: number;
};
export type CState = 'empty'|'miss'|'hit'|'ship';
export type SeaLine = Array<CState>;
export type Sea = Array<SeaLine>;
export type Game = {
  mySea: Sea;
  enemySea: Sea;
  myHit?: Point;
  enemyHit?: Point;
  mode: 'config'|'play';
  enemy: 'sparki'|'human';
  currentBoard?: 'my'|'enemy';
  currentPos?: Point;
}
