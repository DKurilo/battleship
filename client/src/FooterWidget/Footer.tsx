import * as React from 'react';

import styles from './Footer.module.scss';

export const Footer = (props:any) => 
  <div className={styles.Footer}>
    <a href="https://en.m.wikipedia.org/wiki/Battleship_(game)" target="_blank" rel="noopener noreferrer">Battelship</a>:{' '} 
    <a href="https://en.wikipedia.org/wiki/Functional_programming" target="_blank" rel="noopener noreferrer">Functional programming</a>, {' '}
    <a href="https://www.haskell.org/" target="_blank" rel="noopener noreferrer">Haskell</a>, {' '}
    <a href="http://snapframework.com/" target="_blank" rel="noopener noreferrer">Snap Framework</a>,{' '}
    <a href="https://reactjs.org/" target="_blank" rel="noopener noreferrer">React</a>, {' '}
    <a href="https://rxjs-dev.firebaseapp.com/" target="_blank" rel="noopener noreferrer">rxjs</a>, {' '}
    <a href="https://ramdajs.com/" target="_blank" rel="noopener noreferrer">Ramda</a>, {' '}
    <a href="https://www.mongodb.com/" target="_blank" rel="noopener noreferrer">MongoDB</a><br/>
    Author: Dima Kurilo &lt;dkurilo@gmail.com&gt;<br/>
    Github: <a href="https://github.com/DKurilo/battleship" target="_blank" rel="noopener noreferrer">https://github.com/DKurilo/battleship</a>
  </div>
