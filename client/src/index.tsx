import * as React from 'react';
import * as ReactDOM from 'react-dom';
import * as Types from './types';

import { EnemySea } from './EnemySea';
import { Footer } from './Footer';
import { MySea } from './MySea';
import { Settings } from './Settings';
import { Comp } from './Utils';

import './index.css';
import registerServiceWorker from './registerServiceWorker';

const cellStateCycles: any = {
  my: {
    config: {
      empty: 'ship',
      ship: 'empty',
    },
    play: {
      empty: 'miss',
      miss: 'empty',
      ship: 'hit',
      hit: 'ship',
    },
  },
  enemy: {
    config: {
      empty: 'empty',
    },
    play: {
      empty: 'miss',
      miss: 'hit',
      hit: 'empty',
    },
  },
};

const initSea: (x: number, y: number) => Types.Sea = (x, y) => Array(y).fill(0).map(_ => Array(x).fill('empty'));

const init: (x: number, y: number) => Types.Game = (x, y) => ({
  enemy: 'sparki',
  enemySea: initSea(x, y),
  mode: 'config',
  mySea: initSea(x, y),
});

const clickMySea: (g:Types.Game) => (x: number, y: number) => void = g => (x, y) => 
  render(Object.assign(g, {mySea: g.mySea.map((v, x1) => 
    x === x1 ? v.map((v1, y1) => 
      y === y1 ? 
        (cellStateCycles.my[g.mode][g.mySea[x][y]] ? cellStateCycles.my[g.mode][g.mySea[x][y]] : 'empty') : v1) : v)}));

const clickEnemySea: (g:Types.Game) => (x: number, y: number) => void = g => (x, y) =>
  render(Object.assign(g, {enemySea: g.enemySea.map((v, x1) => 
    x === x1 ? v.map((v1, y1) => 
      y === y1 ? 
        (cellStateCycles.enemy[g.mode][g.enemySea[x][y]] ? cellStateCycles.enemy[g.mode][g.enemySea[x][y]] : 'empty') : v1) : v)}));

const config: (g: Types.Game) => () => void = g => () => render(Object.assign(g, {mode: 'config'}));

const toggleEnemy: (g: Types.Game) => () => void = g => () => 
  render(Object.assign(g, {enemy: g.enemy === 'sparki' ? 'human' : 'sparki'}));

const play: (g: Types.Game) => () => void = g => () => render(Object.assign(g, {mode: 'play'}));

const reset: () => void = () => render(init(10, 10));

const mouseEnter: (g: Types.Game) => (board: 'my'|'enemy') => (x: number, y: number) => void = g => board => (x, y) => 
  render(Object.assign(g, {
    currentBoard: board,
    currentPos: {x, y},
  }));

const mouseLeave: (g: Types.Game) => (board: 'my'|'enemy') => (x: number, y: number) => void = g => board => (x, y) => 
  render(Object.assign(g, {
    currentBoard: undefined,
    currentPos: undefined,
  }));

const title: (g:Types.Game) => React.ReactElement<any> = g => <h1>Human vs {g.enemy === 'sparki' ? 'Sparki' : 'human'}!</h1>;

const settings: (g:Types.Game) => React.ReactElement<any> = g => 
  <Settings
    switchToConfig={config(g)}
    switchToPlay={play(g)}
    toggleEnemy={toggleEnemy(g)}
    reset={reset}
    mode={g.mode}
    enemy={g.enemy} 
  />;

const coords: (g:Types.Game) => React.ReactElement<any> = g =>
  <div className="Coords">
    {g.currentPos ? (g.enemy === 'sparki' ? 
      [g.currentPos.x, g.currentPos.y].join('') : 
      [String.fromCharCode(65 + g.currentPos.x), g.currentPos.y + 1].join('')) : ''}
  </div>;

const mySea: (g:Types.Game) => React.ReactElement<any> = g => 
  <MySea 
    clickMySea={clickMySea(g)}
    mySea={g.mySea}
    mouseEnter={mouseEnter(g)('my')}
    mouseLeave={mouseLeave(g)('my')}
    selected={g.currentBoard && g.currentBoard === 'my'}
    selectedPos={g.currentPos}
    enemy={g.enemy}
  />;

const enemySea: (g:Types.Game) => React.ReactElement<any> = g => 
  <EnemySea 
    clickEnemySea={clickEnemySea(g)}
    enemySea={g.enemySea}
    mouseEnter={mouseEnter(g)('enemy')}
    mouseLeave={mouseLeave(g)('enemy')}
    selected={g.currentBoard && g.currentBoard === 'enemy'}
    selectedPos={g.currentPos}
    enemy={g.enemy}
  />;

const footer: (g:Types.Game) => React.ReactElement<any> = g => <Footer />;

const render = (game: Types.Game) => 
  ReactDOM.render(
    Comp(title)
    .concat(Comp(settings))
    .concat(Comp(coords))
    .concat(Comp(mySea))
    .concat(Comp(enemySea))
    .concat(Comp(footer))
    .fold(game),
    document.getElementById('root') as HTMLElement
  );

reset();

registerServiceWorker();
