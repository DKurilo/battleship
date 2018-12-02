import * as React from 'react';
import * as R from 'ramda';

import { Comp } from '../Utils';
import { Rule } from '../types';

import styles from './Help.scss';
import './qm.png';
import right from './right.png';
import wrong1 from './wrong1.png';
import wrong2 from './wrong2.png';
import wrong3 from './wrong3.png';

const formatRules: (s:string) => {__html: string} = s => ({__html: s.replace(/\n/g, '<br/>')});

const currentRules: (p: Rule|undefined) => React.ReactElement<any> = R.ifElse(
  R.compose(R.not, R.isNil),
  x => <React.Fragment>
         <h3>What to place</h3>
         Your current rules are {x.name}.<br/>
         Detailed description:<br/>
         <div className="rules-description" dangerouslySetInnerHTML={formatRules(x.rules)} />
       </React.Fragment>,
  _ => <React.Fragment />
);

export const Help = (props:{rules: Rule|undefined}) => 
  <div className={styles.Help}>
    <div className="qm" />
    <div className="inner">
      <h2>Rules</h2>
      Place your ships, click "Send map", then wait for your opponent. Then try to win.
      <h3>How to place</h3>
      To place one cell click on the map. To remove, click one more time on the same place.<br/>
      These are wrong placements:<br/>
      <img src={wrong1} alt="Wrong!" /><img src={wrong2} alt="Wrong!" /><img src={wrong3} alt="Wrong!" /><br/>
      This is right placement:<br/>
      <img src={right} alt="Right!" />
      {Comp(currentRules).fold(props.rules)}
      <h3>Further reading</h3>
      <a href="https://en.m.wikipedia.org/wiki/Battleship_(game)" target="_blank">Wikipedia page for Battleship Game.</a>
      <h2>Interface</h2>
      <h3>First screen</h3>
      Create new game button. Click to start a new game.<br/>
      List of games. There is the list of games that are public.
      For each game you can join as a guest or a player if another player didn't join yet.
      <h3>Game screen</h3>
      <h4>Header</h4>
      In the header you can find:<br/>
      Back button. Click to exit the game.
      To return to the current game save your URL.
      Game is available for one hour after creation.<br/>
      Your name. Just in case you forgot it.<br/>
      Current rules (underlined). 
      Move your mouse over rules name or tap it to see the full description. 
      Tap on empty space to close rules.<br/>
      Current state. Config or who is shooting now.<br/>
      Link to current game and Copy button. 
      Click on Copy button to copy link in clipboard to send it someone to play together.<br/>
      Invite bot. Click to invite bot if it's possible for your game.<br/>
      <h3>Sea</h3>
      When in Config state you can place and remove your ships by clicking on the sea. When in play state you must shoot!
      <h3>Chat</h3>
      Messages about the game or from your opponents. You can write your own messages. To greet your opponent, as example.
      <h3>Who is here</h3>
      List of persons who are watching your game. Everyone from this list can also write messages in chat.<br/><br/>
      Move your mouse out of this field or tap on empty space to close help section.
    </div>
  </div>
