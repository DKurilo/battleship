import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Guests } from './Guests';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Guests />, div);
  ReactDOM.unmountComponentAtNode(div);
});
