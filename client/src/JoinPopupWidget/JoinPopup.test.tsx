import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { JoinPopup } from './JoinPopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<JoinPopup close={_=>({})}
                             join={_=>_=>({})}
                             changeName={_=>({})}
                             changeMessage={_=>({})}
                             name={'Test game'}
                             message={'Hello world!'}
                             turn={'pre'}
                             error={''} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
