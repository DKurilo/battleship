import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Title } from './Title';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Title />, div);
  ReactDOM.unmountComponentAtNode(div);
});
