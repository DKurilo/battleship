import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { MakeGamePublicPopup } from './MakeGamePublicPopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<MakeGamePublicPopup />, div);
  ReactDOM.unmountComponentAtNode(div);
});
