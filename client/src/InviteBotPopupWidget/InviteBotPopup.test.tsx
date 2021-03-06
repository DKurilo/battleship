import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { InviteBotPopup } from './InviteBotPopup';

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<InviteBotPopup close={_=>({})}
                                  invitebot={_=>_=>({})}
                                  bots={[]}
                                  rules={'starwars'}
                                  error={''} />, div);
  ReactDOM.unmountComponentAtNode(div);
});
