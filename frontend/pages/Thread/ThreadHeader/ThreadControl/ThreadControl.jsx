import React from "react"
import Link from "next/link"

import { cogwheel } from "frontend/assets/svg/cogwheel"
import { document } from "frontend/assets/svg/document"
import { lock     } from "frontend/assets/svg/lock"
 


export function ThreadControl(props) {

  const t = props.t;
  const thread = props.thread;
  const openReplyBox = props.openReplyBox;



  return(
    <div className="ThreadControl">
      { thread.locked
      ? <button
          className="ThreadControl__button ThreadControl__button--locked"
          type="button"
          disabled={true}
        >
          {lock}
          <span>{t("lockedThread")}</span>
        </button>
      : <button
          className="ThreadControl__button ThreadControl__button--newPost"
          type="button"
          onClick={openReplyBox}
        >
          {document}
          <span>{t("newPost")}</span>
        </button>
      }
      <Link href={`/thread/edit?threadId=${thread._id}`}>
        <a className="ThreadControl__button ThreadControl__button--configure">
          {cogwheel}
          <span>{t("configureThread")}</span>
        </a>
      </Link>
    </div>
  )
}