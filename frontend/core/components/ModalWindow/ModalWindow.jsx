import React, {useEffect, useRef} from "react";

import { cancel } from "frontend/assets/svg/cancel";



export function ModalWindow(props) {

  const isActive   = props.isActive;
  const setActive  = props.setActive;
  const title      = props.title;
  const content    = props.children;


  const modalRef = useRef(null);



  function handleClickOutside(event) {
    if (modalRef.current && !modalRef.current.contains(event.target)) {
        setActive(false);
    }
  }

  useEffect(() => {
    const close = (e) => {
      if(e.keyCode === 27){
        setActive(false)
      }
    }
    window.addEventListener('keyup', close)
    return () => window.removeEventListener('keyup', close)
  })




  return(
    isActive &&
    <div className="ModalWindow__outer" onClick={handleClickOutside}>
      <div className="ModalWindow" ref={modalRef}>

        <div className="ModalWindow__heading">
          <button type="button" onClick={() => setActive(false)}>{cancel}</button>
        </div>

        <div className="ModalWindow__contentWrapper">
          <h3 className="ModalWindow__title">
            {title}
          </h3>
          {content}
        </div>

      </div>
    </div>
  )
}