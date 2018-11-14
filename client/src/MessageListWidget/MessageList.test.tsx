import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { MessageList } from './MessageList';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<MessageList />, div);
  ReactDOM.unmountComponentAtNode(div);
});
