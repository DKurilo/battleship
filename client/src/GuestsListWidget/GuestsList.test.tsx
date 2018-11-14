import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { GuestsList } from './GuestsList';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<GuestsList />, div);
  ReactDOM.unmountComponentAtNode(div);
});
