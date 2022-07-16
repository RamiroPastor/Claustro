import React from "react"

import { archive  } from "frontend/assets/svg/archive"
import { document } from "frontend/assets/svg/document"
import { cogwheel } from "frontend/assets/svg/cogwheel"



export function ForumHeader(props) {

  const t = props.t;
  const setModalActive_newBoard = props.setModalActive_newBoard

  return(
    <div className="ForumHeader">
      <button
        type="button"
        className="ForumHeader__button"
        onClick={() => setModalActive_newBoard(true)}
      >
        {document}
        {t("newBoard")}
      </button>
      <button
        type="button"
        className="ForumHeader__button"
      >
        {cogwheel}
        {t("forumSettings")}
      </button>
      <button
        type="button"
        className="ForumHeader__button"
      >
        {archive}
        {t("viewArchive")}
      </button>
    </div>
  )
}