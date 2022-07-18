import React from "react"
import Link from "next/link"

import { lock } from "frontend/assets/svg/lock"
import { pin  } from "frontend/assets/svg/pin"



export function ThreadMiniature(props) {

  const thread = props.thread;
  const userList = userList;



  return(
    <div className="ThreadMiniature">
      <div className="ThreadMiniature__state">
        <div className="ThreadMiniature__iconBox">
          {thread.locked ? lock : ""}
        </div>
        <div className="ThreadMiniature__iconBox">
          {(thread.pinned > 0) ? pin : ""}
        </div>
      </div>
      <div className="ThreadMiniature__info">
        <Link href={`/thread/${thread._id}`}>
          <a className="ThreadMiniature__link">
            {thread.title}
          </a>
        </Link>
        <p className="ThreadMiniature__description">
          {thread.description}
        </p>
      </div>
    </div>
  )
}