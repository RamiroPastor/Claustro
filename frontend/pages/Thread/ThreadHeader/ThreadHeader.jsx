import React from "react"
import Link from "next/link"

import { ThreadControl   } from "./ThreadControl/ThreadControl";
import { ThreadPaginator } from "./ThreadPaginator/ThreadPaginator";



export function ThreadHeader(props) {

  const board  = props.board;
  const thread = props.thread;



  return(
    <div className="ThreadHeader">
      <div className="ThreadHeader__nav">
        <Link href={`/#${board._id}`}>
          <a className="ThreadHeader__navLink">
            {board.title}
          </a>
        </Link>
        <span>&gt;</span>
        <p>{thread.title}</p>
      </div>
      <div className="ThreadHeader__controls">
        <ThreadControl/>
        <ThreadPaginator/>
      </div>
    </div>
  )
}