import * as React from 'react';
import * as R from 'ramda';
import * as Types from '../types';

import { Comp, concat } from '../Utils';

import '../assets/back.png';
import styles from './CreateGamePopup.module.scss';

const formatRules: (s:any) => {__html: string} = s => ({__html: s.replace(/\n/g, '<br/>')});

const wrapAction: (f: Function) => (id: string) => (_:any) => any = f => id => _ => f(id);

const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const option: (f: Function) => (r:Types.Rule) => (cr:string) => React.ReactElement<any> = 
  f => r => cr => 
    <option value={r.id}>{r.name}</option>;

const ruleDescription: (cr: string) => (rules: Array<Types.Rule>) => React.ReactElement<any> = 
  cr => R.compose(R.ifElse(R.compose(R.both(R.compose(R.not, R.isNil), R.is(String)), R.view(R.lensProp('id'))),
    x => <div className={styles['rules-description']} dangerouslySetInnerHTML={formatRules(R.view(R.lensProp('rules'))(x))} />,
    _ => <React.Fragment />
  ), R.find(R.propEq('id', cr)));

const error: (e:string) => React.ReactElement<any> = 
  R.ifElse(R.compose(R.not, R.either(R.isNil, R.isEmpty)),
    x => <div className={styles.error}>{x}</div>,
    _ => <React.Fragment />
  );

export const CreateGamePopup = (props:{close: (e:React.MouseEvent<HTMLDivElement>) => any,
                                       create: (e:React.MouseEvent<HTMLDivElement>) => any,
                                       changeName: (e:React.FormEvent<HTMLInputElement>) => any,
                                       changeMessage: (e:React.FormEvent<HTMLTextAreaElement>) => any,
                                       changeRules: (e:React.FormEvent<HTMLSelectElement>) => any,
                                       rulessets: Array<Types.Rule>
                                       name: string,
                                       message: string,
                                       rules: string,
                                       error: string}) => 
  <div className={styles.CreateGamePopup}>
    <div className={styles.box}>
      <div className={styles.back} onClick={props.close}></div>
      <h3>Create new game</h3>
      <div className={styles.block}>
        <input className={styles.name} placeholder="Your name" value={props.name} onChange={props.changeName} required/>
      </div>
      <div className={styles.block}>
        <textarea className={styles.message} placeholder="Your message (optional)" value={props.message} 
                  onChange={props.changeMessage} required/>
      </div>
      <div className={styles['block-rules']}>
        <select className={styles.rules} placeholder="Your Rules" value={props.rules ? props.rules : 0} 
                  onChange={props.changeRules} required>
          <option disabled={true} value={0}>Your Rules</option>
          {R.reduce(concat, Comp(empty), R.map(R.compose(Comp, option(wrapAction(props.changeRules))), props.rulessets))
            .fold(props.rules)}
        </select>
        {Comp(ruleDescription(props.rules)).fold(props.rulessets)}
      </div>
      <div className={styles.block}>
        {Comp(error).fold(props.error)}
        <div className={styles.button} onClick={props.create}>Create a game</div>
      </div>
    </div>
  </div>
