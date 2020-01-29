import * as React from 'react';

import '../assets/back.png';
import styles from './AreYouSurePopup.module.scss';

export const AreYouSurePopup = (props:{close: (e:React.MouseEvent<HTMLDivElement>) => any,
                                       closeGame: (e:React.MouseEvent<HTMLDivElement>) => any}) =>
  <div className={styles.AreYouSurePopup}>
    <div className={styles.box}>
      <div className={styles.back} onClick={props.close}></div>
      <h3>Are you sure you want to exit?</h3>
      <div className={styles.block}>
        Think wisely.<br/>
        This action is not revertable.<br/>
        You cannot step into the same river twice.
      </div>
      <div className={styles.block}>
        <div className={styles.button} onClick={props.close}>No, wait!</div>
        <div className={styles.button} onClick={props.closeGame}>Yes!</div>
      </div>
    </div>
  </div>
