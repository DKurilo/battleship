import * as React from 'react';

import styles from './Title.scss';

export const Title = (props:any) => 
  <h1 className={styles.Title}>
    {props.text}
  </h1>
