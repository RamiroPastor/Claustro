import React from "react"
import { useTranslation } from "next-i18next"

import { ThreadHeader } from "./ThreadHeader/ThreadHeader";


export function Thread(props) {

  const board  = props.board;
  const thread = props.thread;
  const posts  = props.postList;
  const users  = props.userList

  const t = useTranslation("common").t;



  return(
    <div className="Thread">
      <div className="Thread__inner">
        <ThreadHeader
          board={board}
          thread={thread}
        />
        {posts.map((p,i) =>
          <div key={i}>
            {p.body}
          </div> 
        )}
        <h3>{users[0].name}</h3>
      </div>
    </div>
  )
}