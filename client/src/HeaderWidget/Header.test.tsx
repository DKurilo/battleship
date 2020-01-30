import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Header } from './Header';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Header close={_=>({})}
                          game={{
                                  rules: 'test',
                                  turn: 'notready',
                                  you: 'owner',
                                  yournae: 'Test Testov',
                                  game: '123456789',
                                  message: 'Test message',
                                  owner: {
                                           name: 'Test Testov',
                                           message: 'Hello world!'
                                         },
                                  isPublic: false
                                }}
                          rules={{
                                   rules: 'battleship/four-decker/four-funnel: size: 4, amount: 1\ncruiser/three-decker/three-funnel: size: 3, amount: 2\ndestroyer/two-decker/two-funnel: size: 2, amount: 3\nsubmarine/single-decker/single-funnel: size: 1, amount: 4',
                                   name: 'Star Wars',
                                   id: 'starwars',
                                   order: 0
                                 }}
                          makePublic={_=>({})}
                          inviteBot={_=>({})}
                          session="987654321"/>, div);
  ReactDOM.unmountComponentAtNode(div);
});
