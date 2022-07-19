import React from "react"
import Link from "next/link"

import { cogwheel } from "frontend/assets/svg/cogwheel"
import { document } from "frontend/assets/svg/document"
 


export function ThreadControl(props) {

  const t = props.t;
  const thread = props.thread;



  return(
    <div className="ThreadControl">
      <button
        className="ThreadControl__button ThreadControl__button--newPost"
        type="button"
      >
        {document}
        <span>{t("newPost")}</span>
      </button>
      <Link href={`/thread/edit?threadId=${thread._id}`}>
        <a className="ThreadControl__button ThreadControl__button--configure">
          {cogwheel}
          <span>{t("configureThread")}</span>
        </a>
      </Link>
    </div>
  )
}