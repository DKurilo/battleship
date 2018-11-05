import * as React from 'react';

import './Battleships.css';

import logo from './logo.svg';

const Battleships = (props:any) => {
  const parseIntSafe: (x:any) => number = x => parseInt('' + x, 10) || 0;
  const clickHandler: ((e:React.MouseEvent<HTMLImageElement>) => void ) = 
    e => props.clickSea(
      parseIntSafe(e.currentTarget.dataset.x), 
      parseIntSafe(e.currentTarget.dataset.y), 
      props.state
    );

  return (
    <div className="Battleships">
      <header className="Battleships-header">
        <img src={logo} className="Battleships-logo" alt="logo" onClick={clickHandler} data-x={0} data-y={0} />
        <h1 className="Battleships-title">Welcome to Battleships</h1>
      </header>
      <p className="Battleships-intro">
        {props.mySea}
      </p>
    </div>
  );
};

export default Battleships;
