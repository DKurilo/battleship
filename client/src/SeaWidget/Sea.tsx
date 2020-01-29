import * as React from 'react';
import * as R from 'ramda';
import * as Types from '../types';

import { Comp } from '../Utils';

import './empty.png';
import './hit.png';
import './label-empty.png';
import './label-x.png';
import './label-y.png';
import './miss.png';
import './ship.png';
import './wait.png';
import styles from './Sea.module.scss';

const classByVal = ['empty', 'ship', 'miss', 'hit', 'wait'];

const bottom: (p: {you: string,
                   message: string
                   sendMap: ((_:React.MouseEvent<HTMLDivElement>) => any) | null}) => React.ReactElement<any> = 
  R.ifElse(R.compose(R.not, R.propEq('you', 'guest')),
    x => <div className={styles['bottom-block']}>
           {Comp(message).fold(x.message)}
           {Comp(sendMapButtton).fold(x.sendMap)}
         </div>,
    _ => <React.Fragment />
  );

const message: (e:string) => React.ReactElement<any> = 
  R.ifElse(R.compose(R.not, R.either(R.isNil, R.isEmpty)),
    x => <div className={styles.message}>{x}</div>,
    _ => <React.Fragment />
  );

const sendMapButtton: (f:((_:React.MouseEvent<HTMLDivElement>) => any) | null) => React.ReactElement<any> = 
  R.ifElse(R.compose(R.not, R.isNil),
    x => <div className={styles.send} onClick={x}>Send map</div>,
    _ => <React.Fragment />
  );

const cell: (i: number) => Types.Box = i => Comp(g =>
  <div 
    className={`${styles.Cell} ${styles[classByVal[g.line[i]]]}` + 
               (g.selectedPos === i || g.selected ? ` ${styles.light}` : '') +
               (g.selectedPos === i && g.selected ? ` ${styles.current}` : '') }
    onClick={g.click?g.click(i):(_=>null)}
    onMouseEnter={g.enter(i)}
    onMouseLeave={g.leave(i)}
  />
);
const seaLine: (s: Types.SeaLine) => Types.Box = 
  s => s.reduce((acc, i, y) => acc.concat(cell(y)), Comp(g => <React.Fragment />));
const SeaLine = (props: any) => seaLine(props.line).fold(props);

const rightLabels: (s: Types.SeaLine) => Types.Box = s => s.reduce((acc, i, y) => acc.concat(Comp(g =>
    <div className={`${styles.Label} ${styles.y}` + (g.selected && g.selectedPos && g.selectedPos.y === y ? ` ${styles.active}` : '') }>
      {y + 1}
    </div>
  )), Comp(g => <React.Fragment />));

const rightLine: (s: Types.SeaLine) => Types.Box = s => Comp(g => 
  <div className={styles.SeaLine}><div className={`${styles.Label} ${styles.empty}`} />{rightLabels(s).fold(g)}</div>);

const sea: (s: Types.Sea) => Types.Box = s => s.reduce((acc, i, x) => acc.concat(Comp(g =>
    <div className={styles.SeaLine}>
      <div className={`${styles.Label} ${styles.x}` + (g.selected && g.selectedPos && g.selectedPos.x === x ? ` ${styles.active}` : '')}>
        {String.fromCharCode(65 + x)}
      </div>
      <SeaLine 
        click={g.changeMap?g.changeMap(x):(g.shoot?g.shoot(x):null)}
        enter={g.enter(x)}
        leave={g.leave(x)}
        line={g.sea[x]}
        selected={g.selected && g.selectedPos && g.selectedPos.x === x}
        selectedPos={g.selected && g.selectedPos ? g.selectedPos.y : undefined}/>
    </div>
  )), Comp(g => <React.Fragment />));

export const Sea = (props:{sendMap: ((_:React.MouseEvent<HTMLDivElement>) => any) | null,
                           changeMap: ((x:number) => (y:number) => (_:React.MouseEvent<HTMLDivElement>) => any)|null,
                           shoot: ((x:number) => (y:number) => (_:React.MouseEvent<HTMLDivElement>) => any)|null,
                           enter: (x:number) => (y:number) => (_:React.MouseEvent<HTMLDivElement>) => any,
                           leave: (x:number) => (y:number) => (_:React.MouseEvent<HTMLDivElement>) => any,
                           you: Types.You,
                           turn: Types.Turn,
                           name: string,
                           selected: boolean,
                           selectedPos: Types.Point,
                           sea: Types.Sea,
                           message: string|undefined}) => 
  <div className={styles.Sea}>
    <h3>{props.name ? `${props.name}'s Sea` : 'Waiting...'}</h3>
    <div className={styles.Sea}>{rightLine(props.sea[0]).concat(sea(props.sea)).fold(props)}</div>
    {Comp(bottom).fold(R.pickAll(['you','message','sendMap'], props))}
  </div>;
