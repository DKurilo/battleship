import * as React from 'react';

import styles from './CreateGame.scss';

export const CreateGame = (props:{action: (e:React.MouseEvent<HTMLDivElement>) => void}) => 
  <div className={styles.CreateGame} onClick={props.action}>
    Create new game
  </div>
