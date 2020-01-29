import * as React from 'react';
import * as R from 'ramda';
import * as Types from '../types';

import { Comp, concat } from '../Utils';

import styles from './PublicGamesList.module.scss';

const formatRules: (s:string) => {__html: string} = s => ({__html: s.replace(/\n/g, '<br/>')});

const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const wrapAction: (f: Function) => (id: string) => (_:any) => any = f => id => _ => f(id);

const oops: (g: Array<any>) => React.ReactElement<any> = R.ifElse(R.either(R.isEmpty, R.isNil),
    _ => <p className={styles['no-games']}>Oops! So empty. You can create one!</p>,
    empty
  );

const game: (f: Function) => (g: Types.PublicGame) => (rules:Array<Types.Rule>) => React.ReactElement<any> = 
  f => g => rules => ((r:Types.Rule) => 
    <div className={styles.game}>
      <div>
        <span className={styles.label}>Owner: </span><span className={styles.owner}>{g.owner}</span>
      </div>
      <div className={styles.label}>Message:</div>
      <div className={styles.message}>{g.message}</div>
      <div className={styles.rules}>
        <span className={styles.label}>Rules: </span><span className={styles['rules-name']}>{r.name}
          <div className={styles['rules-description']} dangerouslySetInnerHTML={formatRules(r.rules)} />
        </span>
      </div>
      <div>
        <div className={styles.button} onClick={f(g.game)}>Join</div>
      </div>
    </div>)(R.find(R.propEq('id', g.rules))(rules));

export const PublicGamesList = (props:{games: Array<Types.PublicGame>, rules: Array<Types.Rule>, action: Function}) =>
  <div className={styles.PublicGamesList}>
    <h3>Or join one of these games:</h3>
    {Comp(oops).fold(props.games)}
    {R.reduce(concat, Comp(empty), R.map(R.compose(Comp, game(wrapAction(props.action))), props.games)).fold(props.rules)}
  </div>
