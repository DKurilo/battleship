import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { PublicGamesList } from './PublicGamesList';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<PublicGamesList games={[]}
                                   rules={[]}
                                   action={_=>({})}/>, div);
  ReactDOM.unmountComponentAtNode(div);
});
