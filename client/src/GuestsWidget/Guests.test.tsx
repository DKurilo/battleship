import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { Guests } from './Guests';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<Guests guests={[]}
                          owner={{
                                    name: 'Test Testov',
                                    message: 'Hello world!'
                                  }}
                          player={undefined} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
