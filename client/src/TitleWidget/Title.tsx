import * as React from 'react';

const styles = require('./Title.module.scss');

export const Title = (props:any) => 
  <h1 className={styles.Title}>
    {props.text}
  </h1>
