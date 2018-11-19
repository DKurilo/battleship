import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Chat } from './Chat';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Chat />, div);
  ReactDOM.unmountComponentAtNode(div);
});
