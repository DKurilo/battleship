import * as React from 'react';
import * as Types from '../types';

import { Comp } from '../Utils';

import './Sea.css';

import './empty.png';
import './hit.png';
import './label-empty.png';
import './label-x.png';
import './label-y.png';
import './miss.png';
import './ship.png';

const cell: (i: number) => Types.Box = i => Comp(g =>
  <div 
    className={'Cell ' + g.line[i] + 
               (g.selectedPos === i || g.selected ? ' light' : '') +
               (g.selectedPos === i && g.selected ? ' current' : '') }
    onClick={g.click(i)}
    onMouseEnter={g.enter(i)}
    onMouseLeave={g.leave(i)}
  />
);
const seaLine: (s: Types.SeaLine) => Types.Box = 
  s => s.reduce((acc, i, y) => acc.concat(cell(y)), Comp(g => <React.Fragment />));
const SeaLine = (props: any) => seaLine(props.line).fold(props);

const rightLabels: (s: Types.SeaLine) => Types.Box = s => s.reduce((acc, i, y) => acc.concat(Comp(g =>
    <div className={'Label y' + (g.selected && g.selectedPos && g.selectedPos.y === y ? ' active' : '') }>{g.enemy === 'sparki' ? y : (y + 1)}</div>
  )), Comp(g => <React.Fragment />));

const rightLine: (s: Types.SeaLine) => Types.Box = s => Comp(g => 
  <div className="SeaLine"><div className="Label empty" />{rightLabels(s).fold(g)}</div>);

const sea: (s: Types.Sea) => Types.Box = s => s.reduce((acc, i, x) => acc.concat(Comp(g =>
    <div className="SeaLine"><div className={'Label x' + (g.selected && g.selectedPos && g.selectedPos.x === x ? ' active' : '')}>{g.enemy === 'sparki' ? x : String.fromCharCode(65 + x)}</div><SeaLine 
        click={g.click(x)}
        enter={g.enter(x)}
        leave={g.leave(x)}
        line={g.sea[x]}
        selected={g.selected && g.selectedPos && g.selectedPos.x === x}
        selectedPos={g.selected && g.selectedPos ? g.selectedPos.y : undefined}
        enemy={g.enemy}
      /></div>
  )), Comp(g => <React.Fragment />));

export const Sea = (props:any) => <div className="Sea">{rightLine(props.sea[0]).concat(sea(props.sea)).fold(props)}</div>;
