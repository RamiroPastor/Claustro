import React from "react"
import Link from "next/link"

import { ThreadControl   } from "./ThreadControl/ThreadControl";
import { ThreadPaginator } from "./ThreadPaginator/ThreadPaginator";



export function ThreadHeader(props) {

  const t = props.t;
  const board  = props.board;
  const thread = props.thread;
  const dir = props.dir



  return(
    <div className="ThreadHeader" style={{flexDirection: dir}}>
      <div className="ThreadHeader__nav">
        <Link href={`/#${board._id}`} scroll={false}>
          <a className="ThreadHeader__navLink">
            {board.title}
          </a>
        </Link>
        <span>
          &gt;
        </span>
        <p className="ThreadHeader__navCurrent">
          {thread.title}
        </p>
      </div>
      <div className="ThreadHeader__controls">
        <ThreadControl
          t={t}
          thread={thread}
        />
        <ThreadPaginator/>
      </div>
    </div>
  )
}