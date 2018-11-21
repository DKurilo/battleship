import * as React from 'react';
import * as R from 'ramda';
import * as Types from '../types';

import { Comp, concat } from '../Utils';

import styles from './PublicGamesList.scss';

const formatRules: (s:string) => {__html: string} = s => ({__html: s.replace(/\n/g, '<br/>')});

const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const wrapAction: (f: Function) => (id: string) => (_:any) => any = f => id => _ => f(id);

const oops: (g: Array<any>) => React.ReactElement<any> = R.ifElse(R.either(R.empty, R.isNil),
    _ => <p className="no-games">Oops! So empty. You can create one!</p>,
    empty
  );

const game: (f: Function) => (g: Types.PublicGame) => (rules:Array<Types.Rule>) => React.ReactElement<any> = 
  f => g => rules => ((r:Types.Rule) => 
    <div className="game">
      <div>
        <span className="label">Owner: </span><span className="owner">{g.owner}</span>
      </div>
      <div className="label">Message:</div>
      <div className="message">{g.message}</div>
      <div className="rules">
        <span className="label">Rules: </span><span className="rules-name">{r.name}
          <div className="rules-description" dangerouslySetInnerHTML={formatRules(r.rules)} />
        </span>
      </div>
      <div>
        <div className="button" onClick={f(g.game)}>Join</div>
      </div>
    </div>)(R.find(R.propEq('id', g.rules))(rules));

export const PublicGamesList = (props:{games: Array<Types.PublicGame>, rules: Array<Types.Rule>, action: Function}) =>
  <div className={styles.PublicGamesList}>
    <h3>Or join one of these games:</h3>
    {Comp(oops).fold(props.games)}
    {R.reduce(concat, Comp(empty), R.map(R.compose(Comp, game(wrapAction(props.action))), props.games)).fold(props.rules)}
  </div>
