import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { CreateGamePopup } from './CreateGamePopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<CreateGamePopup />, div);
  ReactDOM.unmountComponentAtNode(div);
});
