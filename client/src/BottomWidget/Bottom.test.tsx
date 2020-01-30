import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Bottom } from './Bottom';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Bottom elements={[{name: 'Test', component:_=><React.Fragment />}]}
                          battle={{mode:'pre', api: '', bottom: 0}}
                          change={_=>_=>({})} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
