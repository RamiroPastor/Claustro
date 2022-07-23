import React from "react"

import { archive  } from "frontend/assets/svg/archive"
import { document } from "frontend/assets/svg/document"
import { cogwheel } from "frontend/assets/svg/cogwheel"



export function ForumHeader(
  props:
    { t : (s: string) => string
    , openModal : () => void
    }
  ) {

  const t = props.t;
  const openModal = props.openModal

  return(
    <div className="ForumHeader">
      <button
        type="button"
        className="ForumHeader__button"
        onClick={openModal}
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