import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { EnemySea } from './EnemySea';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<EnemySea />, div);
  ReactDOM.unmountComponentAtNode(div);
});
