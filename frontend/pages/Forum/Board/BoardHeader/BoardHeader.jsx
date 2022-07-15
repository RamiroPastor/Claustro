import React from "react"

import { archive  } from "frontend/assets/svg/archive"
import { document } from "frontend/assets/svg/document"
import { cogwheel } from "frontend/assets/svg/cogwheel"



export function BoardHeader(props) {

  const t = props.t;

  return(
    <div className="BoardHeader">
      <button
        type="button"
        className="BoardHeader__button"
      >
        {document}
        {t("newForum")}
      </button>
      <button
        type="button"
        className="BoardHeader__button"
      >
        {cogwheel}
        {t("forumSettings")}
      </button>
      <button
        type="button"
        className="BoardHeader__button"
      >
        {archive}
        {t("viewArchive")}
      </button>
    </div>
  )
}