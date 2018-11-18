import * as React from 'react';
import * as R from 'ramda';
import { of } from 'rxjs';
import { tap } from 'rxjs/operators';
import * as Types from '../types';

import { Comp } from '../Utils';

import styles from './Header.scss';
import '../assets/back.png';

const innerState:{el?: Element} = {};

const getLink:(gid: string) => string = gid => `${window.location.protocol}//${window.location.host}/games/${gid}`;

const formatRules: (s:string) => {__html: string} = s => ({__html: s.replace(/\n/g, '<br/>')});

const getState: (t:string) => string = t => ({
  notready: 'config',
  config: 'config',
  owner: 'play',
  player: 'play',
  owner_win: 'finished',
  palyer_win: 'finished',
}[t]);

const setLinkElement:(el:HTMLInputElement) => any  =
  el => Object.assign(innerState, { el: el });

const copyLink:(_:React.MouseEvent<HTMLDivElement>) => any = 
  _ => R.compose(R.when(R.compose(R.not, R.isNil),
    e => of(1).pipe(
      tap(_ => e.select())
    ).subscribe(
      _ => document.execCommand('copy')
    )
  ), R.view(R.lensProp('el')))(innerState);

const makePublicButton: (p: any) => React.ReactElement<any> = R.ifElse(R.view(R.lensPath(['game', 'isPublic'])),
  _ => <React.Fragment />,
  x => <div className="make-public" onClick={x.makePublic}>Make public</div>
);

export const Header = (props:{close: (e:React.MouseEvent<HTMLDivElement>) => any,
                              rules: Types.Rule,
                              game: Types.Game,
                              makePublic: (e:React.MouseEvent<HTMLDivElement>) => any}) => 
  <div className={styles.Header}>
    <div className="close" onClick={props.close} />
    <div className="right">
      <div className="rules-box">
        <div className="rules">{props.rules.name}</div>
        <div className="rules-description" dangerouslySetInnerHTML={formatRules(props.rules.rules)} />
      </div>
      <div className="state">{getState(props.game.turn)}</div>
      <div className="copylink">
        <input type="text" className="link" ref={setLinkElement} value={getLink(props.game.game)} readOnly={true}/>
        <div className="copy" onClick={copyLink}>Copy</div>
      </div>
      {Comp(makePublicButton).fold(props)}
    </div>
  </div>
