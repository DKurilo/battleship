import * as React from 'react';

import * as Types from '../types';

import { Sea } from '../SeaWidget';
import { Comp } from '../Utils';

import styles from './ConfigurableSea.scss';


const clickHandler: (func: (x: number, y: number) => void) => (x: number) => (y: number) => (e:React.MouseEvent<HTMLImageElement>) => void = 
  func => x => y => e => func(x, y);
const mouseEnterHandler: (func: (x: number, y: number) => void) => (x: number) => (y: number) => (e:React.MouseEvent<HTMLImageElement>) => void = 
  func => x => y => e => func(x, y);
const mouseLeaveHandler: (func: (x: number, y: number) => void) => (x: number) => (y: number) => (e:React.MouseEvent<HTMLImageElement>) => void = 
  func => x => y => e => func(x, y);
const sea: (s: Types.Props) => React.ReactElement<any> = s => 
  <Sea
    click={clickHandler(s.clickMySea)}
    enter={mouseEnterHandler(s.mouseEnter)}
    leave={mouseLeaveHandler(s.mouseLeave)}
    sea={s.mySea}
    selected={s.selected}
    selectedPos={s.selectedPos}
    enemy={s.enemy}
  />;

export const ConfigurableSea = (props:any) => 
  <div className={styles.ConfigurableSea}><h3 className={styles.h3}>Place your ships</h3>
    {Comp(sea).fold(props)}
  </div>;
