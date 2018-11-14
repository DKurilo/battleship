import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { GuestSea } from './GuestSea';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<GuestSea />, div);
  ReactDOM.unmountComponentAtNode(div);
});
