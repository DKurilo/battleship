import * as React from 'react';
import * as Types from '../types';

import { Comp } from '../Utils';

import styles from './Bottom.module.scss';

export const Bottom = (props:{elements: Array<Types.TabItem>,
                              battle: Types.Battleship
                              change: (i: number) => (_:React.MouseEvent<HTMLDivElement>) => any}) => 
  <div className={styles.Bottom}>
    <div className={styles['top-line']}>
      {props.elements.map((e, i) => 
        <div key={`${i}`} className={styles.element + (props.battle.bottom === i ? ` ${styles.active}` : '')} onClick={props.change(i)}>{e.name}</div>)}
    </div>
    {Comp(props.elements[props.battle.bottom ? props.battle.bottom : 0].component).fold(props.battle)}
  </div>
