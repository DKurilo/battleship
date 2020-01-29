import * as React from 'react';
import * as R from 'ramda';
import * as Types from '../types';

import { Comp, concat } from '../Utils';

import styles from './Guests.module.scss';

const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const h3: (t: string) => (o: any) => React.ReactElement<any> = 
  t => R.ifElse(R.both(R.compose(R.not, R.isNil), R.compose(R.not, R.isEmpty)),
    _ => <h3>{t}</h3>,
    _ => <React.Fragment />
  )

const player: (p: Types.Person) => React.ReactElement<any> = 
  R.ifElse(R.both(R.compose(R.not, R.isNil), R.compose(R.not, R.isEmpty)),
    x => <div className={styles.player}>
            <div className={styles.name}>{x.name}</div>
            <div className={styles.message}>{x.message}</div>
          </div>,
    _ => <React.Fragment />);

const guest: (p: Types.Person) => (_:any) => React.ReactElement<any> = 
  R.ifElse(R.compose(R.not, R.isNil),
    x => (_: any) => <div className={styles.guest}>
                <div className={styles.name}>{x.name}</div>
                <div className={styles.message}>{x.message}</div>
              </div>,
    _ => (_: any) => <React.Fragment />);

export const Guests = (props:{guests: Array<Types.Person>, owner: Types.Person, player: Types.Person | undefined}) =>
  <div className={styles.Guests}>
    {Comp(h3('First player:')).concat(Comp(player)).fold(props.owner)}
    {Comp(h3('Second player:')).concat(Comp(player)).fold(props.player)}
    {Comp(h3('Guests:')).fold(props.guests)}
    {R.reduce(concat, Comp(empty), R.map(R.compose(Comp, guest), props.guests)).fold({})}
  </div>
