import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Bottom } from './Bottom';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Bottom />, div);
  ReactDOM.unmountComponentAtNode(div);
});
