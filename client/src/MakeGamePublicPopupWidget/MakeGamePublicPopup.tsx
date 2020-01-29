import * as React from 'react';
import * as R from 'ramda';

import { Comp } from '../Utils';

import '../assets/back.png';
import styles from './MakeGamePublicPopup.module.scss';

const error: (e:string) => React.ReactElement<any> = 
  R.ifElse(R.compose(R.not, R.either(R.isNil, R.isEmpty)),
    x => <div className={styles.error}>{x}</div>,
    _ => <React.Fragment />
  );

export const MakeGamePublicPopup = (props:{close: (e:React.MouseEvent<HTMLDivElement>) => any,
                                           makepublic: (e:React.MouseEvent<HTMLDivElement>) => any,
                                           changeMessage: (e:React.FormEvent<HTMLTextAreaElement>) => any,
                                           message: string,
                                           error: string}) =>
  <div className={styles.MakeGamePublicPopup}>
    <div className={styles.box}>
      <div className={styles.back} onClick={props.close}></div>
      <h3>Make game public</h3>
      <div className={styles.block}>
        After you do this, game will appear on main page and anyone will be able to join as player or as guest.<br/>
        If you just want to invite your friend copy link from header (just click Copy to copy it) and send it to your friend.
      </div>
      <div className={styles.block}>
        <textarea className={styles.message} placeholder="Your message" value={props.message} 
                  onChange={props.changeMessage} required/>
      </div>
      <div className={styles.block}>
        {Comp(error).fold(props.error)}
        <div className={styles.button} onClick={props.makepublic}>Do it!</div>
      </div>
    </div>
  </div>
