import React from "react"
import Link from "next/link"

import { BoardResData } from "centre/Board/BoardResData";
import { ThreadResData } from "centre/Thread/ThreadResData";
import { ThreadControl   } from "./ThreadControl/ThreadControl";



export function ThreadHeader(
  props:
    { t            : (s: string) => string
    , board        : BoardResData
    , thread       : ThreadResData
    , flexDir      : React.CSSProperties
    , openReplyBox : () => void
    , Paginator    : () => React.ReactElement
    }
  ) {

  const t            = props.t;
  const board        = props.board;
  const thread       = props.thread;
  const flexDir      = props.flexDir;
  const openReplyBox = props.openReplyBox;
  const Paginator    = props.Paginator;



  return(
    <div className="ThreadHeader" style={flexDir}>
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
          openReplyBox={openReplyBox}
        />
        <Paginator/>
      </div>
    </div>
  )
}