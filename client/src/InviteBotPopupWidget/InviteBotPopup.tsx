import * as React from 'react';
import * as R from 'ramda';

import { Comp, concat } from '../Utils';

import * as Types from '../types';

import styles from './InviteBotPopup.scss';
import '../assets/back.png';

const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const error: (e:string) => React.ReactElement<any> = 
  R.ifElse(R.compose(R.not, R.either(R.isNil, R.isEmpty)),
    x => <div className="error">{x}</div>,
    _ => <React.Fragment />
  );

const button: (f: Function) => (bot: Types.Bot) => (rules: string) => React.ReactElement<any> = 
  f => bot => R.ifElse(R.flip(R.contains)(R.prop('rules')(bot)),
    _ => <div className="button" onClick={f(bot.name)}>{bot.name}</div>,
    _ => <React.Fragment />);

export const InviteBotPopup = (props:{close: (e:React.MouseEvent<HTMLDivElement>) => any,
                                           invitebot: (name:string) => (e:React.MouseEvent<HTMLDivElement>) => any,
                                           bots: Array<Types.Bot>,
                                           rules: string,
                                           error: string}) =>
  <div className={styles.InviteBotPopup}>
    <div className="box">
      <div className="back" onClick={props.close}></div>
      <div className="block">
        Click on Bot name to invite it.
      </div>
      <div className="bots-list">
        {R.reduce(concat, Comp(empty), R.map(R.compose(Comp, button(props.invitebot)), props.bots)).fold(props.rules)}
      </div>
      <div className="block">
        {Comp(error).fold(props.error)}
      </div>
    </div>
  </div>
