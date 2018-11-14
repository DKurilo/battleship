import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { MySea } from './MySea';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<MySea />, div);
  ReactDOM.unmountComponentAtNode(div);
});
