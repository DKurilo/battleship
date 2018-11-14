import * as React from 'react';
import * as R from 'ramda';
import { RxHR } from '@akanass/rx-http-request';
import { of } from 'rxjs';
import { tap } from 'rxjs/operators';
import * as ReactDOM from 'react-dom';

import * as Types from './types';

import { Loader } from './LoaderWidget';
import { Title } from './TitleWidget';
import { CreateGame } from './CreateGameWidget';
import { PublicGamesList } from './PublicGamesListWidget';
import { JoinPopup } from './JoinPopupWidget';
import { Footer } from './FooterWidget';
import { Comp, concat } from './Utils';

import './index.css';
import registerServiceWorker from './registerServiceWorker';

// Initial data structure
const initialBattleship: Types.Battleship = {
  mode: 'pre',
  api: 'http://localhost:9000/api/games'
}

const getGameId: () => string = () => R.ifElse(
  R.compose(R.lt(1), R.length),
  path => path[1],
  () => ''
)(window.location.pathname.split('/'));
const getSessionId: () => string = () => R.ifElse(
  R.compose(R.lt(2), R.length),
  path => path[2],
  () => ''
)(window.location.pathname.split('/'));

// Elements
const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const loader: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'pre'}),
   x => <Loader />,
   x => <React.Fragment />
);

const title: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'init'}),
   x => <Title />,
   x => <React.Fragment />
);

const publicgames: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'init'}),
   x => <PublicGamesList />,
   x => <React.Fragment />
);

const creategame: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'init'}),
   x => <CreateGame />,
   x => <React.Fragment />
);

const joinpopup: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'join'}),
   x => <JoinPopup />,
   x => <React.Fragment />
);

const footer: (g:Types.Battleship) => React.ReactElement<any> = g => <Footer />;

//Render. Should be very simple.
const render = (game: Types.Battleship) => 
  ReactDOM.render(R.reduce(concat, Comp(empty), R.map(Comp, 
    [title, creategame, publicgames, joinpopup, footer, loader]
  )).fold(game), document.getElementById('root') as HTMLElement);

// Actions
const init: (battle:Types.Battleship) => void = battle => RxHR.get(battle.api + '/rules', {json: true}).pipe(
  tap(_ => render(battle))
).subscribe(R.when(R.compose(R.equals(200),R.view(R.lensPath(['response', 'statusCode']))),
  R.compose(rulesets => R.ifElse(R.isEmpty,
    () => renderInit(Object.assign(battle, {mode: 'init', rules: rulesets})),
    gameid => RxHR.get(battle.api + '/' + gameid, {json: true}).subscribe(
      R.ifElse(R.compose(R.equals(200),R.view(R.lensPath(['response', 'statusCode']))),
      R.compose(game => render(Object.assign(battle, {mode: 'join', rules: rulesets, join:game})), R.view(R.lensProp('body'))),
      R.compose(_ => renderInit(Object.assign(battle, {mode: 'init', rules: rulesets})), R.view(R.lensProp('body'))))),
    )(getGameId()), R.view(R.lensProp('body')))));

const renderInit: (battle:Types.Battleship) => void = battle => RxHR.get(battle.api, {json: true}).subscribe(
  R.when(R.compose(R.equals(200),R.view(R.lensPath(['response', 'statusCode']))),
  R.compose(games => render(Object.assign(battle, {mode: 'init', init: games})), R.view(R.lensProp('body')))));


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
