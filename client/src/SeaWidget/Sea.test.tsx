import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Sea } from './Sea';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Sea sendMap={null}
                       changeMap={_=>_=>_=>({})}
                       shoot={null}
                       enter={_=>_=>_=>({})}
                       leave={_=>_=>_=>({})}
                       you={'owner'}
                       turn={'notready'}
                       name={'Test game'}
                       selected={false}
                       selectedPos={{x:0, y:0}}
                       sea={[[0,0],[0,0]]}
                       message={''} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
