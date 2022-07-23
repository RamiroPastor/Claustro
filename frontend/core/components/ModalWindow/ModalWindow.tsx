import React, {useEffect, useRef} from "react";

import { cancel } from "frontend/assets/svg/cancel";



export function ModalWindow(
  props:
    { isActive   : boolean
    , closeModal : () => void
    , title      : string
    , children   : React.ReactNode
    }
  ) {

  const isActive   = props.isActive;
  const closeModal = props.closeModal;
  const title      = props.title;
  const content    = props.children;


  const modalRef = useRef<HTMLDivElement>(null);



  function handleClickOutside(e: React.MouseEvent) {
    const target = e.target as HTMLElement
    if (modalRef.current && !modalRef.current.contains(target)) {
        closeModal();
    }
  }

  useEffect(() => {
    const close = (e: KeyboardEvent) => {
      const key = e.key as string
      if(key === "Escape"){
        closeModal()
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
          <button type="button" onClick={closeModal}>{cancel}</button>
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