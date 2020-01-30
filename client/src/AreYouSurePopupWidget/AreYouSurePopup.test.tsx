import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { AreYouSurePopup } from './AreYouSurePopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<AreYouSurePopup close={_=>({})} closeGame={_=>({})} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
