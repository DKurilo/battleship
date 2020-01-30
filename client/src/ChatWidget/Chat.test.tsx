import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Chat } from './Chat';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Chat messages={undefined}
                        sendMessage={_=>({})}
                        changeMessage={_=>({})}
                        message={'Test'} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
