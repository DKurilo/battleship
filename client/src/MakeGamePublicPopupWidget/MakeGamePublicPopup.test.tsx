import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { MakeGamePublicPopup } from './MakeGamePublicPopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<MakeGamePublicPopup close={_=>({})}
                                       makePublic={_=>({})}
                                       changeMessage={_=>({})}
                                       message={'Hello world!'}
                                       error={''} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
