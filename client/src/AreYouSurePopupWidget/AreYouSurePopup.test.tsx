import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { AreYouSurePopup } from './AreYouSurePopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<AreYouSurePopup />, div);
  ReactDOM.unmountComponentAtNode(div);
});
