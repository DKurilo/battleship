import * as React from 'react';

import styles from './Footer.scss';

export const Footer = (props:any) => 
  <div className={styles.Footer}>
    <a href="https://en.m.wikipedia.org/wiki/Battleship_(game)" target="_blank">Battelship</a>:{' '} 
    <a href="https://en.wikipedia.org/wiki/Functional_programming" target="_blank">Functional programming</a>, {' '}
    <a href="https://www.haskell.org/" target="_blank">Haskell</a>, {' '}
    <a href="http://snapframework.com/" target="_blank">Snap Framework</a>,{' '}
    <a href="https://reactjs.org/" target="_blank">React</a>, {' '}
    <a href="https://rxjs-dev.firebaseapp.com/" target="_blank">rxjs</a>, {' '}
    <a href="https://ramdajs.com/" target="_blank">Ramda</a>, {' '}
    <a href="https://www.mongodb.com/" target="_blank">MongoDB</a><br/>
    Author: Dima Kurilo &lt;dkurilo@gmail.com&gt;<br/>
    Github: <a href="https://github.com/DKurilo/battleship" target="_blank">https://github.com/DKurilo/battleship</a>
  </div>
