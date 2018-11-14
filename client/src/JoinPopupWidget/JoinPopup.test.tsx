import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { JoinPopup } from './JoinPopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<JoinPopup />, div);
  ReactDOM.unmountComponentAtNode(div);
});
