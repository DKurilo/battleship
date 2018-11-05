import * as React from 'react';
import * as ReactDOM from 'react-dom';
import Battleships from './Battleships';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Battleships />, div);
  ReactDOM.unmountComponentAtNode(div);
});
