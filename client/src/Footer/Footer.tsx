import * as React from 'react';

import './Footer.css';

export const Footer = (props:any) => 
  <div className="Footer">
    This page was created as a helper for Battleship <a href="http://arcbotics.com/products/sparki/" target="_blank">game for Sparki</a>. Source for game is <a href="https://github.com/DKurilo/SparkiForIlya/tree/master/battleships" target="_blank">here</a>.<br/>
    So the default map field namings are 0-9 for x-axis and 0-9 for y-axis. Sparki doesn't have letters on remote.<br/>
    But you can use more typical naming when you are playing with your friends. Just click Toggle Enemy and play in Human vs Human version.<br/>
    It can work in two modes:<br/>
    <ol>
      <li>Config</li>
      <li>Play</li>
    </ol>
    In Config mode you can place your ships. Just click on cells where you want to have a ship. First click places a ship, second removes a ship from a cell. After all ships are placed, click Play.<br/>
    In Play mode if your click on your ship it will be marked as a hit, if you click on an empty cell it will be marked as a miss. Second click will revert it. If you click on the enemy sea, the cell will be marked as a miss, after the second click it will be marked as a hit and on the third click it will be empty again.<br/>
    Click Reset if you want to clear everything.<br/>
    <br/>
    Sparki prefer StarWars version, so:
    <ul>
      <li>four-decker (4 cells) - 1</li>
      <li>three-decker (3 cells) - 2</li>
      <li>two-decker (2 cells) - 3</li>
      <li>one-decker (1 cells) - 4</li>
    </ul>
    Ships can be placed horizontally or vertically and can't be bent.<br/>
    You can use your version if you are playing with your friends.
  </div>
