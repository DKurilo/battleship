import * as React from 'react';
import * as R from 'ramda';
import { of } from 'rxjs';
import { tap } from 'rxjs/operators';
import * as Types from '../types';

import { Comp } from '../Utils';

import '../assets/back.png';
import styles from './Header.module.scss';

const innerState:{el?: Element} = {};

const getLink:(gid: string) => string = gid => `${window.location.protocol}//${window.location.host}/games/${gid}`;

const formatRules: (s:string) => {__html: string} = s => ({__html: s.replace(/\n/g, '<br/>')});

const getState: (o:string) => (p:string) => (t:string) => string = o => p => t => (({
  notready: 'config',
  config: 'config',
  owner: `${o}'s turn`,
  player: `${p}'s turn`,
  owner_win: `${o} won!`,
  player_win: `${p} won!`,
} as {[key: string]:string})[t]);

const setLinkElement:(el:HTMLInputElement) => any  =
  el => Object.assign(innerState, { el: el });

const copyLink:(_:React.MouseEvent<HTMLDivElement>) => any = 
  _ => R.compose(R.when(R.compose(R.not, R.isNil),
    e => of(1).pipe(
      tap(_ => e.focus()),
      tap(_ => e.setSelectionRange(0,10000))
    ).subscribe(
      _ => document.execCommand('copy')
    )
  ), R.view(R.lensProp('el')))(innerState);

const makePublicButton: (p: any) => React.ReactElement<any> = R.ifElse(R.view(R.lensPath(['game', 'isPublic'])),
  _ => <React.Fragment />,
  x => <div className={styles['make-public']} onClick={x.makePublic}>Make public</div>
);

const inviteBotButton: (p: any) => React.ReactElement<any> = R.ifElse(
  R.compose(R.isNil, R.view(R.lensPath(['game', 'player', 'name']))),
  x => <div className={styles['invite-bot']} onClick={x.inviteBot}>Invite bot</div>,
  _ => <React.Fragment />
);

export const Header = (props:{close: (e:React.MouseEvent<HTMLDivElement>) => any,
                              rules: Types.Rule,
                              game: Types.Game,
                              makePublic: (e:React.MouseEvent<HTMLDivElement>) => any,
                              inviteBot: (e:React.MouseEvent<HTMLDivElement>) => any,
                              session:string}) => 
  <div className={styles.Header}>
    <div className={styles.close} onClick={props.close} />
    <div className={styles.youare}>You are: <b>{props.game.yourname}</b></div>
    <div className={styles.right}>
      <div className={styles['rules-box']}>
        <div className={styles.rules}>{props.rules.name}</div>
        <div className={styles['rules-description']} dangerouslySetInnerHTML={formatRules(props.rules.rules)} />
      </div>
      <div className={styles.state}>{
        getState(props.game.owner.name)(props.game.player ? props.game.player.name : '')(props.game.turn)
      }</div>
      <div className={styles.copylink}>
        <input type="text" className={styles.link} ref={setLinkElement} value={getLink(props.game.game)} contentEditable="true"/>
        <div className={styles.copy} onClick={copyLink}>Copy</div>
      </div>
      {Comp(makePublicButton).concat(Comp(inviteBotButton)).fold(props)}
    </div>
  </div>
