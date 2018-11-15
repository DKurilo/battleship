import * as React from 'react';
import * as R from 'ramda';
import * as Types from '../types';

import { Comp, concat } from '../Utils';

import styles from './PublicGamesList.scss';

const formatRules: (s:string) => {__html: string} = s => ({__html: s.replace(/\n/g, '<br/>')});

const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const game: (g: Types.PublicGame) => (rules:Array<Types.Rule>) => React.ReactElement<any> = g => rules =>
  ((r:Types.Rule) => 
    <div className="game">
      <div className="owner">{g.owner}</div>
      <div className="message">{g.message}</div>
      <div className="rules">
        <div className="rule-name">{r.name}</div>
        <div className="rules-description" dangerouslySetInnerHTML={formatRules(r.rules)} />
      </div>
    </div>)(R.find(R.propEq('id', g.rules))(rules));

export const PublicGamesList = (props:{games: Array<Types.PublicGame>, rules: Array<Types.Rule>}) =>
  <div className={styles.PublicGamesList}>
    <h3>Here is the list of public games you can join.</h3>
    {R.reduce(concat, Comp(empty), R.map(R.compose(Comp, game), props.games)).fold(props.rules)}
  </div>
