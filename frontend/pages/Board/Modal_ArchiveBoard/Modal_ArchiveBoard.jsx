import React from "react"

import { ModalWindow  } from "frontend/core/components/ModalWindow/ModalWindow"



export function Modal_ArchiveBoard(props) {

  const t = props.t;
  const isActive = props.isActive;
  const setActive = props.setActive;



  return(
    <ModalWindow
      isActive   = {isActive}
      setActive  = {setActive}
      title      = {t("archiveBoard")}
    >
      <div className="Modal_ArchiveBoard">
        <strong className="Modal_ArchiveBoard__caution">
          {t("caution")}
        </strong>
        <p>
          {t("thisWillCloseThreads")}
          <br/>
          {t("andArchiveTheBoard")}
        </p>
        <p>
          <em>{t("youSure")}</em>
        </p>
        <b>
          ({t("notImplementedYet")})
        </b>
        <div className="Modal_ArchiveBoard__buttons">
          <button
            className="Modal_ArchiveBoard__button"
            type="button"
            onClick={() => setActive(false)}
          >
            {t("doNotArchiveBoard")}
          </button>
          <button
            className="Modal_ArchiveBoard__button Modal_ArchiveBoard__button--archive"
            type="button"
            onClick={() => alert(t("notImplementedYet"))}
          >
            {t("archiveBoard")}
          </button>
        </div>
      </div>
    </ModalWindow>
  )
}