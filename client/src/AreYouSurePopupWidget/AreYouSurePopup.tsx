import * as React from 'react';

import styles from './AreYouSurePopup.scss';
import '../assets/back.png';

export const AreYouSurePopup = (props:{close: (e:React.MouseEvent<HTMLDivElement>) => any,
                                       closeGame: (e:React.MouseEvent<HTMLDivElement>) => any}) =>
  <div className={styles.AreYouSurePopup}>
    <div className="box">
      <div className="back" onClick={props.close}></div>
      <h3>Are you sure you want to exit?</h3>
      <div className="block">
        Think wisely.<br/>
        This action is not revertable.<br/>
        You cannot step into the same river twice.
      </div>
      <div className="block">
        <div className="button" onClick={props.close}>No, wait!</div>
        <div className="button" onClick={props.closeGame}>Yes!</div>
      </div>
    </div>
  </div>
