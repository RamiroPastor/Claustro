import React from "react"
import Link from "next/link"

import { lock } from "frontend/assets/svg/lock"
import { pin  } from "frontend/assets/svg/pin"



export function ThreadMiniature(props) {

  const t = props.t;
  const thread = props.thread;
  const userList = props.userList;

  const userName_creation = 
    userList.find(u => u._id === thread.createdByUser).name;
  const userName_lastActivity =
    userList.find(u => u._id === thread.lastActivity.userId).name;



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
        <div className="ThreadMiniature__moreInfo">
          <span>
            {t("created")}
            <em>{thread.createdAt.slice(0,10)}</em>
            {t("by")}
            <em>{userName_creation}</em>
          </span>
          <span>
            {t("lastActivity")}
            <em>{thread.lastActivity.date.slice(0,10)}</em>
            {t("by")}
            <em>{userName_lastActivity}</em>
          </span>
        </div>
      </div>
    </div>
  )
}