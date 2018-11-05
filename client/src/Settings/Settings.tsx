import * as React from 'react';

import './Settings.css';

const handler: (func: () => void) => (e:React.MouseEvent<HTMLImageElement>) => void = func => e => func();

export const Settings = (props:any) => 
  <div className="Settings">
    <div onClick={handler(props.switchToConfig)} className={props.mode === 'config' ? 'selected' : ''}>Config</div>
    <div onClick={handler(props.switchToPlay)} className={props.mode === 'play' ? 'selected' : ''}>Play</div>
    <div onClick={handler(props.reset)}>Reset</div>
    <div onClick={handler(props.toggleEnemy)}>Toggle Enemy</div>
  </div>
