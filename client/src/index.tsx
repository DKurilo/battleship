import * as React from 'react';
import * as R from 'ramda';
import { of } from 'rxjs';
import { ajax } from 'rxjs/ajax';
import { tap, delay } from 'rxjs/operators';
import * as ReactDOM from 'react-dom';

import * as Types from './types';

import { Loader } from './LoaderWidget';
import { Title } from './TitleWidget';
import { CreateGame } from './CreateGameWidget';
import { PublicGamesList } from './PublicGamesListWidget';
import { JoinPopup } from './JoinPopupWidget';
import { Footer } from './FooterWidget';
import { Comp, concat } from './Utils';

import './index.scss';
import registerServiceWorker from './registerServiceWorker';

/*****************
*
* You need to understand, battle is mutable object.
* Each time you are using Object.assign it's mutated
* It's not pure, but it allow us to have multiple threads
* and to work with games list and with messages
* 
******************/

// Initial data structure
const initialBattleship: Types.Battleship = {
  mode: 'pre',
  api: 'http://localhost:9000/api/games'
}

const getGameId: () => string = () => R.ifElse(
  R.allPass([R.compose(R.lt(2), R.length), R.compose(R.equals("games"), R.view(R.lensIndex(1)))]),
  path => path[2],
  () => ''
)(window.location.pathname.split('/'));
const getSessionId: () => string = () => R.ifElse(
  R.allPass([R.compose(R.lt(2), R.length), R.compose(R.equals("games"), R.view(R.lensIndex(1)))]),
  path => path[3],
  () => ''
)(window.location.pathname.split('/'));

// Utils
const checkMode: (a:Array<string>) => (g:Types.Battleship) => boolean = 
  a => R.compose(R.flip(R.contains)(a), R.view(R.lensProp('mode')));

//'pre'|'init'|'create'|'join'|'loading'|'game'|'make_public'
const generateTitle: (b:Types.Battleship) => string = b => ({
  pre: "Battleship Game",
  init: "Battleship Game",
  create: "Battleship Game",
  join: "Battleship Game",
  game: b.game && b.game.player ? b.game.owner.name + " vs " + b.game.player.name : 'Battleship Game',
  make_public: b.game && b.game.player ? b.game.owner.name + " vs " + b.game.player.name : 'Battleship Game',
}[b.mode]);  

// Elements
const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const loader: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'pre'}),
  x => <Loader />,
  _ => <React.Fragment />
);

const title: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  checkMode(['init', 'join', 'create']),
  x => <Title text={generateTitle(x)}/>,
  _ => <React.Fragment />
);

const publicgames: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  checkMode(['init', 'join', 'create']),
  x => <PublicGamesList games={x.init} rules={x.rules} />,
  _ => <React.Fragment />
);

const creategame: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  checkMode(['init', 'join', 'create']),
  x => <CreateGame />,
  _ => <React.Fragment />
);

const joinpopup: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'join'}),
  x => <JoinPopup />,
  _ => <React.Fragment />
);

const footer: (g:Types.Battleship) => React.ReactElement<any> = g => <Footer />;

//Render. Should be very simple.
const render = (game: Types.Battleship) => 
  ReactDOM.render(R.reduce(concat, Comp(empty), R.map(Comp, 
    [title, creategame, publicgames, joinpopup, footer, loader]
  )).fold(game), document.getElementById('root') as HTMLElement);

// Actions
const init: (battle:Types.Battleship) => any = battle => ajax.getJSON(battle.api + '/rules').pipe(
  tap(_ => render(battle))
).subscribe(
  rulesets => R.ifElse(R.isEmpty,
    () => renderInit(Object.assign(battle, {mode: 'init', rules: rulesets})),
    gameid => ajax.getJSON(battle.api + '/' + gameid).subscribe(
      game => renderInit(Object.assign(battle, {mode: 'join', rules: rulesets, join:game})),
      _ => renderInit(Object.assign(battle, {mode: 'init', rules: rulesets}))),
    )(getGameId())
);

const renderInit: (battle:Types.Battleship) => any = battle => ajax.getJSON(battle.api).pipe(
  tap(_ => R.when(checkMode(['init', 'join', 'create']), b => 
    of(1).pipe(delay(4000)).subscribe(x=>renderInit(b)))(battle))
).subscribe(
  games => render(Object.assign(battle, {init: games})),
  _ => R.when(checkMode(['init', 'join', 'create']), b => 
    of(1).pipe(delay(4000)).subscribe(x=>renderInit(b)))(battle)
);

// Entry point
/************ render(Object.assign(battle, {mode: 'init', rules: data})
/* On init we need to get rules, check for GameId in hash, 
/* if we have gameId to try to get game.
/* If game available, next status is join
/* If there is no game with such ID next status is init
/* if we don't have such gameID, status is init,
************/
const main: () => void = () => of(1).pipe(
  tap(_ => registerServiceWorker()),
  tap(_ => init(initialBattleship))
).subscribe();

main();
