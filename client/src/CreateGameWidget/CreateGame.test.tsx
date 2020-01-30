import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { CreateGame } from './CreateGame';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<CreateGame action={_=>({})} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
