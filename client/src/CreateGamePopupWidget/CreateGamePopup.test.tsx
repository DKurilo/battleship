import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { CreateGamePopup } from './CreateGamePopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<CreateGamePopup close={_=>({})}
                                   create={_=>({})}
                                   changeName={_=>({})}
                                   changeMessage={_=>({})}
                                   changeRules={_=>({})}
                                   rulessets={[{
                                                 rules: 'battleship/four-decker/four-funnel: size: 4, amount: 1\ncruiser/three-decker/three-funnel: size: 3, amount: 2\ndestroyer/two-decker/two-funnel: size: 2, amount: 3\nsubmarine/single-decker/single-funnel: size: 1, amount: 4',
                                                 name: 'Star Wars',
                                                 id: 'starwars',
                                                 order: 0
                                               }]}
                                   name={'Test game'}
                                   message={'Message'}
                                   rules={'starwars'}
                                   error={''}/>, div);
  ReactDOM.unmountComponentAtNode(div);
});
