import * as React from 'react';
import * as R from 'ramda';
import { of } from 'rxjs';
import { ajax } from 'rxjs/ajax';
import { tap, delay } from 'rxjs/operators';
import * as ReactDOM from 'react-dom';

import * as Types from './types';

import { AreYouSurePopup } from './AreYouSurePopupWidget';
import { Bottom } from './BottomWidget';
import { Chat } from './ChatWidget';
import { CreateGamePopup } from './CreateGamePopupWidget';
import { CreateGame } from './CreateGameWidget';
import { Footer } from './FooterWidget';
import { Guests } from './GuestsWidget';
import { Header } from './HeaderWidget';
import { JoinPopup } from './JoinPopupWidget';
import { Loader } from './LoaderWidget';
import { MakeGamePublicPopup } from './MakeGamePublicPopupWidget';
import { PublicGamesList } from './PublicGamesListWidget';
import { Sea } from './SeaWidget';
import { Title } from './TitleWidget';
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
  api: `${window.location.protocol}//${window.location.host}/api/games`
}

const X:number = 10;
const Y:number = 10;

// Utils
const initSea: (x: number, y: number) => Types.Sea = (x, y) => Array(y).fill(0).map(_ => Array(x).fill(0));


const emptySea:Types.Sea = initSea(X, Y);

const getGameId: () => string = () => R.ifElse(
  R.allPass([R.compose(R.lt(2), R.length), R.compose(R.equals('games'), R.view(R.lensIndex(1)))]),
  path => path[2],
  () => ''
)(window.location.pathname.split('/'));

const getSessionId: () => string = () => R.ifElse(
  R.allPass([R.compose(R.lt(2), R.length), R.compose(R.equals('games'), R.view(R.lensIndex(1)))]),
  path => path[3],
  () => ''
)(window.location.pathname.split('/'));

const checkMode: (a:Array<string>) => (g:Types.Battleship) => boolean = 
  a => R.compose(R.flip(R.contains)(a), R.view(R.lensProp('mode')));

const generateGameTitle: (g:Types.Game) => string = g => ({
  notready: 'Waiting for players!',
  config: 'Will start soon!',
  owner: g.player ? g.owner.name + ' vs ' + g.player.name : 'Battleship Game',
  player: g.player ? g.owner.name + ' vs ' + g.player.name : 'Battleship Game',
  owner_win: g.owner.name + ' won!',
  palyer_win: g.player ? g.player.name + ' won!' : 'Battleship Game',
}[g.turn]);

const generateTitle: (b:Types.Battleship) => string = b => ({
  pre: 'Battleship Game',
  init: 'Battleship Game',
  create: 'Battleship Game',
  join: 'Battleship Game',
  game: b.game ? generateGameTitle(b.game) : 'Battleship Game',
  make_public: b.game ? generateGameTitle(b.game) : 'Battleship Game',
  are_you_sure: b.game ? generateGameTitle(b.game) : 'Battleship Game',
}[b.mode]);  

const currentRulesId: (b:Types.Battleship) => string = 
  R.ifElse(R.compose(R.compose(R.not, R.either(R.isEmpty, R.isNil), R.view(R.lensProp('game')))), 
    x => x.game.rules,
    _ => ''
  );

const currentRules: (b:Types.Battleship) => Types.Rule = R.ifElse(R.compose(R.isNil, R.view(R.lensProp('game'))),
  _ => undefined,
  x => R.find(R.propEq('id', currentRulesId(x)))(x.rules)
);

const setWait: (s: Types.Sea) => (x:number) => (y:number) => Types.Sea = 
  s => x => y => s.map((v, x1) => 
      x === x1 ? v.map((v1, y1) => 
        y === y1 ? 4 : v1) : v)

const seaOwner = {
  right: {
    player: 'owner',
    owner: 'player',
    guest: 'player'
  },
  left: {
    player: 'player',
    owner: 'owner',
    guest: 'owner'
  }
};

// Elements
const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const loader: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  checkMode(['pre', 'loading']),
  x => <Loader />,
  _ => <React.Fragment />
);

const header: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  checkMode(['game', 'make_public', 'are_you_sure']),
  x => <Header close={openAreYouSurePopup(x)}
               rules={currentRules(x)}
               game={x.game}
               makePublic={openMakePublicPopup(x)}/>,
  _ => <React.Fragment />
);

const title: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  checkMode(['init', 'join', 'create', 'game', 'make_public', 'are_you_sure']),
  x => <Title text={generateTitle(x)}/>,
  _ => <React.Fragment />
);

const publicgames: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  checkMode(['init', 'join', 'create']),
  x => <PublicGamesList games={x.init} rules={x.rules} action={openJoinPopup(x)}/>,
  _ => <React.Fragment />
);

const creategame: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  checkMode(['init', 'join', 'create']),
  x => <CreateGame action={openCreateGamePopup(x)} />,
  _ => <React.Fragment />
);

const joinpopup: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'join'}),
  x => <JoinPopup close={closeJoinPopup(x)} 
                  join={joinGame(x)}
                  changeName={popupChangeName(x)}
                  changeMessage={popupChangeMessage(x)}
                  name={x.popupName}
                  message={x.popupMessage}
                  turn={x.join.turn}
                  error={x.popupError}/>,
  _ => <React.Fragment />
);

const creategamepopup: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'create'}),
  x => <CreateGamePopup close={closeCreateGamePopup(x)} 
                        create={createGame(x)}
                        changeName={popupChangeName(x)}
                        changeMessage={popupChangeMessage(x)}
                        changeRules={popupChangeRules(x)}
                        name={x.popupName}
                        message={x.popupMessage}
                        error={x.popupError}
                        rules={x.popupRules}
                        rulessets={x.rules}/>,
  _ => <React.Fragment />
);

const makepublicpopup: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'make_public'}),
  x => <MakeGamePublicPopup close={closeMakePublicPopup(x)} 
                            makepublic={makeGamePublic(x)}
                            changeMessage={popupChangeMessage(x)}
                            message={x.popupMessage}
                            error={x.popupError}/>,
  _ => <React.Fragment />
);

const areyousurepopup: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
  R.whereEq({mode: 'are_you_sure'}),
  x => <AreYouSurePopup close={closeAreYouSurePopup(x)} 
                        closeGame={closeGame(x)}/>,
  _ => <React.Fragment />
);

const bottom: (els:Array<Types.TabItem>) => (g:Types.Battleship) => React.ReactElement<any> = 
  els => R.ifElse(
    checkMode(['game', 'make_public', 'are_you_sure']),
    x => <Bottom elements={els} battle={x}/>,
    _ => <React.Fragment />
  );

const chat: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
    checkMode(['game', 'make_public', 'are_you_sure']),
    x => <Chat messages={x.chat}
               sendMessage={sendChatMessage(x)}
               changeMessage={changeChatMessage(x)}/>,
    _ => <React.Fragment />
  );

const guests: (g:Types.Battleship) => React.ReactElement<any> = R.ifElse(
    checkMode(['game', 'make_public', 'are_you_sure']),
    x => <Guests guests={x.game.guests} owner={x.game.owner} player={x.game.player}/>,
    _ => <React.Fragment />
  );

const sea: (s:'right'|'left') => (g:Types.Battleship) => React.ReactElement<any> = s => R.ifElse(
    checkMode(['game', 'make_public', 'are_you_sure']),
    x => <Sea you={x.game.you}
              turn={x.game.turn}
              sendMap={x.game.you!=='guest' && s==='left' && (x.game.turn === 'notready' || x.game.turn === 'config') ? 
                         sendMySea(x):null}
              changeMap={x.game.you!=='guest' && s==='left' && (x.game.turn === 'notready' || x.game.turn === 'config') ? 
                         changeMySea(x):null}
              shoot={x.game.you!=='guest' && s==='right' && x.game.turn === x.game.you ? shoot(x) : null}
              sea={x.game.you!=='guest' && s==='left' && (x.game.turn === 'notready' || x.game.turn === 'config') ?
                   x.initSea : (x.game[seaOwner[s][x.game.you]] && x.game[seaOwner[s][x.game.you]].map ? 
                   x.game[seaOwner[s][x.game.you]].map : emptySea)}
              name={x.game[seaOwner[s][x.game.you]]?x.game[seaOwner[s][x.game.you]].name:''}
              mouseEnter={mouseEnter(x)(s)}
              mouseLeave={mouseLeave(x)(s)}
              selected={x.currentBoard && x.currentBoard === s}
              selectedPos={x.currentPos}/>,
    _ => <React.Fragment />
  );

const footer: (g:Types.Battleship) => React.ReactElement<any> = g => <Footer />;

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

const openJoinPopup: (battle:Types.Battleship) => (gameid: string) => any = 
  battle => gameid => 
  battle.init && 
  render(Object.assign(battle, {mode: 'join', join: R.find(R.propEq('game', gameid))(battle.init)}));

const closeJoinPopup: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  battle => _ => render(Object.assign(battle, {mode: 'init', join: undefined, popupError: undefined}));

const joinGame: (battle:Types.Battleship) => (r:string) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  R.ifElse( R.allPass(
      R.map(x => R.compose(R.not, R.either(R.isEmpty, R.isNil), R.view(R.lensProp(x))),
            ['popupName', 'popupMessage', 'join'])),
    b => (r:string) => (_:React.MouseEvent<HTMLDivElement>) => ajax({
      url: `${b.api}/${b.join.game}/connect/${r}`,
      method: 'POST',
      body: {
        name: b.popupName,
        message: b.popupMessage,
      },
      headers: {
        'Content-Type': 'application/json',
      }
    }).pipe(
      tap(_ => render(Object.assign(b, {mode: 'loading'})))
    ).subscribe (
      r => of(1).pipe(
        tap(_ => renderGame(Object.assign(b, {
          mode: 'game', gameid: r.response.game, session: r.response.session, bottom: 0, initSea: initSea(X, Y)
        }))),
        tap(_ => renderChat(b))
      ).subscribe(),
      _ => renderInit(Object.assign(b, {mode: 'join', popupError: 'Something goes wrong. Try again later.'}))
    ),
    b => (r:string) => (_:React.MouseEvent<HTMLDivElement>) => 
      render(Object.assign(b, {popupError: 'Name and message can\'t be empty.' }))
  );

const openCreateGamePopup: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  battle => _ => render(Object.assign(battle, {mode: 'create'}));

const closeCreateGamePopup: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  battle => _ => render(Object.assign(battle, {mode: 'init', popupError: undefined}));

const createGame: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  R.ifElse( R.allPass(
      R.map(x => R.compose(R.not, R.either(R.isEmpty, R.isNil), R.view(R.lensProp(x))),
            ['popupName', 'popupMessage', 'popupRules'])),
    b => (_:React.MouseEvent<HTMLDivElement>) => ajax({
      url: b.api,
      method: 'POST',
      body: {
        name: b.popupName,
        message: b.popupMessage,
        rules: b.popupRules,
      },
      headers: {
        'Content-Type': 'application/json',
      }
    }).pipe(
      tap(_ => render(Object.assign(b, {mode: 'loading'})))
    ).subscribe (
      r => of(1).pipe(
        tap(_ => renderGame(Object.assign(b, {
          mode: 'game', gameid: r.response.game, session: r.response.session, bottom: 0, initSea: initSea(X, Y)
        }))),
        tap(_ => renderChat(b))
      ).subscribe(),
      _ => renderInit(Object.assign(b, {mode: 'create', popupError: 'Something goes wrong. Try again later.'}))
    ),
    b => (_:React.MouseEvent<HTMLDivElement>) => 
      render(Object.assign(b, {mode: 'create', popupError: 'Name, message and rules can\'t be empty.' }))
  );

const renderGame: (battle:Types.Battleship) => any = 
  battle => ajax.getJSON(`${battle.api}/${battle.gameid}/${battle.session}`).pipe(
    tap(_ => R.when(checkMode(['game', 'make_public', 'are_you_sure']), b => 
      of(1).pipe(delay(1000)).subscribe(x=>renderGame(b)))(battle))
  ).subscribe(
    game => render(Object.assign(battle, {game: game})),
    _ => R.when(checkMode(['game', 'make_public', 'are_you_sure']), b => 
      of(1).pipe(delay(1000)).subscribe(x=>renderGame(b)))(battle)
  );

const renderChat: (battle:Types.Battleship) => any =
  battle => ajax.getJSON(`${battle.api}/${battle.gameid}/${battle.session}/chat`).pipe(
    tap(_ => R.when(checkMode(['game', 'make_public', 'are_you_sure']), b => 
      of(1).pipe(delay(1000)).subscribe(x=>renderChat(b)))(battle))
  ).subscribe(
    chat => render(Object.assign(battle, {chat: chat})),
    _ => R.when(checkMode(['game', 'make_public', 'are_you_sure']), b => 
      of(1).pipe(delay(1000)).subscribe(x=>renderChat(b)))(battle)
  );

const popupChangeName: (battle:Types.Battleship) => (e:React.FormEvent<HTMLInputElement>) => any = 
  battle => e => e.currentTarget.value.length <= 10 && 
                 render(Object.assign(battle, {popupName: e.currentTarget.value, popupError: ''}));

const popupChangeMessage: (battle:Types.Battleship) => (e:React.FormEvent<HTMLTextAreaElement>) => any = 
  battle => e => e.currentTarget.value.length <= 140 && 
                 render(Object.assign(battle, {popupMessage: e.currentTarget.value, popupError: ''}));

const popupChangeRules: (battle:Types.Battleship) => (e:React.FormEvent<HTMLSelectElement>) => any = 
  battle => e => render(Object.assign(battle, {popupRules: e.currentTarget.value, popupError: ''}));

const closeGame: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  battle => _ => renderInit(Object.assign(battle, {mode: 'init'}));

const openMakePublicPopup: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  battle => _ => battle.gameid && render(Object.assign(battle, {mode: 'make_public', popupMessage: ''}));

const closeMakePublicPopup: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  battle => _ => render(Object.assign(battle, {mode: 'game'}));

const makeGamePublic: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  R.ifElse( R.allPass(
      R.map(x => R.compose(R.not, R.either(R.isEmpty, R.isNil), R.view(R.lensProp(x))),
            ['game', 'gameid', 'session', 'popupMessage'])),
    b => (_:React.MouseEvent<HTMLDivElement>) => ajax({
      url: `${b.api}/${b.gameid}/${b.session}/setpublic`,
      method: 'POST',
      body: {
        message: b.popupMessage,
      },
      headers: {
        'Content-Type': 'application/json',
      }
    }).subscribe(
      r => render(Object.assign(b, {mode: 'game'})),
      _ => render(Object.assign(b, {popupError: 'Something goes wrong. Try again later.' }))
    ),
    b => (_:React.MouseEvent<HTMLDivElement>) => 
      render(Object.assign(b, {popupError: 'Message can\'t be empty.' }))
  );

const openAreYouSurePopup: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  battle => _ => battle.gameid && render(Object.assign(battle, {mode: 'are_you_sure'}));

const closeAreYouSurePopup = closeMakePublicPopup;

const changeMySea: (battle:Types.Battleship) => (x:number) => (y:number) => any =
  battle => x => y => battle.initSea && battle.initSea[x] && battle.initSea[x][y] ?
    render(Object.assign(battle, {popupError: '', initSea: battle.initSea.map((v, x1) => 
      x === x1 ? v.map((v1, y1) => 
        y === y1 ? (v1 === 0 ? 1 : 0) : v1) : v)})) : render(battle);

const sendMySea: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  R.ifElse( R.allPass(
      R.map(x => R.compose(R.not, R.either(R.isEmpty, R.isNil), R.view(R.lensProp(x))),
            ['game', 'gameid', 'session', 'initSea'])),
    b => (_:React.MouseEvent<HTMLDivElement>) => ajax({
      url: `${b.api}/${b.gameid}/${b.session}/setmap`,
      method: 'POST',
      body: b.initSea,
      headers: {
        'Content-Type': 'application/json',
      }
    }).subscribe(
      _ => render(b),
      m => render(Object.assign(b, {popupError: m }))
    ),
    b => (_:React.MouseEvent<HTMLDivElement>) => 
      render(Object.assign(b, {popupError: 'Something goes wrong!' }))
  );

const shoot: (battle:Types.Battleship) => (x:number) => (y:number) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  battle => x => y => _ => of(battle).pipe(
    tap(R.ifElse(R.compose(R.equals('owner'), R.view(R.lensPath(['game','you']))), 
      b => render(Object.assign(b, { game: { player: {sea: setWait(b.game.player.sea)(x)(y)}}})),
      b => render(Object.assign(b, { game: { owner: {sea: setWait(b.game.owner.sea)(x)(y)}}}))))
  ).subscribe(
    b => ajax({
        url: `${b.api}/${b.gameid}/${b.session}/setmap`,
        method: 'POST',
        body: {
          x: x,
          y: y
        },
        headers: {
          'Content-Type': 'application/json',
        }
      }).subscribe(
        r => render(Object.assign(b, {message: r})),
        r => render(Object.assign(b, {message: r}))
      )
  );

const changeChatMessage: (battle:Types.Battleship) => (e:React.FormEvent<HTMLInputElement>) => any = 
  battle => e => e.currentTarget.value.length <= 140 && 
                 render(Object.assign(battle, {message: e.currentTarget.value, popupError: ''}));

const sendChatMessage: (battle:Types.Battleship) => (_:React.MouseEvent<HTMLDivElement>) => any = 
  R.when( R.allPass(
      R.map(x => R.compose(R.not, R.either(R.isEmpty, R.isNil), R.view(R.lensProp(x))),
            ['game', 'gameid', 'session', 'message'])),
    b => (_:React.MouseEvent<HTMLDivElement>) => ajax({
      url: `${b.api}/${b.gameid}/${b.session}/chat`,
      method: 'POST',
      body: b.message,
      headers: {
        'Content-Type': 'application/json',
      }
    }).subscribe(r => render(Object.assign(b, {message: ''})))
  );

const mouseEnter: (b: Types.Battleship) => (board: 'right'|'left') => (x: number) => (y: number) 
                  => (_:React.MouseEvent<HTMLDivElement>) => any = 
  b => board => x => y => _ => 
    render(Object.assign(b, {
      currentBoard: board,
      currentPos: {x, y},
    }));

const mouseLeave: (b: Types.Battleship) => (board: 'right'|'left') => (x: number) => (y: number)
                  => (_:React.MouseEvent<HTMLDivElement>) => any = 
  b => board => x => y => _ => 
  render(Object.assign(b, {
    currentBoard: undefined,
    currentPos: undefined,
  }));

//Render. Should be very simple.
const render = (game: Types.Battleship) => 
  ReactDOM.render(R.reduce(concat, Comp(empty), R.map(Comp, 
    [
      header, 
      title,
      creategame,
      publicgames,
      joinpopup,
      creategamepopup,
      makepublicpopup,
      areyousurepopup,
      sea('right'),
      sea('left'),
      bottom([{name: 'Chat', component: chat} , {name: 'Guests List', component: guests}]),
      footer,
      loader]
  )).fold(game), document.getElementById('root') as HTMLElement);

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
