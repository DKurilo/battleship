import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { CreateMessage } from './CreateMessage';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<CreateMessage />, div);
  ReactDOM.unmountComponentAtNode(div);
});
