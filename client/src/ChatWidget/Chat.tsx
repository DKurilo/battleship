import * as React from 'react';
import * as R from 'ramda';
import * as Types from '../types';

import { Comp, concat } from '../Utils';

import styles from './Chat.module.scss';

const empty: (_:any) => React.ReactElement<any> = _ => <React.Fragment />;

const message: (m: Types.Message) => (_:any) => React.ReactElement<any> = 
  m => (_: any) => <div className={styles['chat-message']}>
                     <div className={styles.name}>{m.name}: {' '}</div>
                     <div className={R.join(' ', R.concat([styles.text], [styles[R.defaultTo('justMessage')(R.last(R.match(/.. - (miss|hit|sank)/, m.message)))]]))}>
                       {m.message}
                     </div>
                   </div>;

const handlePressKey: (f:Function) => (e:React.KeyboardEvent<HTMLInputElement>) => any = 
  f => R.when(R.compose(R.equals('Enter'), R.view(R.lensProp('key'))),
    _ => f()
  );

export const Chat = (props:{messages: Array<Types.Message>|undefined,
                            sendMessage: (_:any) => any,
                            changeMessage: (e:React.FormEvent<HTMLInputElement>) => any,
                            message: string}) => 
  <div className={styles.Chat}>
    <div className={styles['message-box']}>
      <input type="text" className={styles.message} value={props.message} 
             onChange={props.changeMessage} onKeyPress={handlePressKey(props.sendMessage)}/>
      <div className={styles.send} onClick={props.sendMessage}>Send</div>
    </div>
    <div className={styles['chat-box']}>
      {R.when(R.compose(R.not, R.isNil), 
        ms => R.reduce(concat, Comp(empty), R.map(R.compose(Comp, message), ms)).fold({}))(props.messages)}
    </div>
  </div>
