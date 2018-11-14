import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ConfigurableSea } from './ConfigurableSea';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<ConfigurableSea />, div);
  ReactDOM.unmountComponentAtNode(div);
});
