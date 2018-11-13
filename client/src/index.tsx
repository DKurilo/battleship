import * as React from 'react';
import * as ReactDOM from 'react-dom';
import * as Types from './types';

import { Footer } from './Footer';
import { Comp } from './Utils';

import './index.css';
import registerServiceWorker from './registerServiceWorker';

const initialBattleship: Types.Battleship = {
  mode: 'pre'
}

const main: (_: boolean) => void = (_) => render(initialBattleship);

const title: (g:Types.Battleship) => React.ReactElement<any> = g => <h1>Hi!</h1>;

const footer: (g:Types.Battleship) => React.ReactElement<any> = g => <Footer />;

const render = (game: Types.Battleship) => 
  ReactDOM.render(
    Comp(title)
    .concat(Comp(footer))
    .fold(game),
    document.getElementById('root') as HTMLElement
  );

main(registerServiceWorker());

