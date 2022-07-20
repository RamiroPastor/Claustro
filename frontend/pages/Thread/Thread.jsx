import React from "react"
import { useTranslation } from "next-i18next"

import { Post } from "frontend/pages/Post/Post"
import { ThreadHeader } from "./ThreadHeader/ThreadHeader"


export function Thread(props) {

  const board  = props.board;
  const thread = props.thread;
  const posts  = props.postList;
  const users  = props.userList

  const t = useTranslation("common").t;



  return(
    <div className="Thread">
      <div className="Thread__background">
        <div className="Thread__inner">
          <ThreadHeader
            t={t}
            board={board}
            thread={thread}
            dir="column"
          />
          {posts.map((p,i) =>
            <Post
              key={i}
              t={t}
              index={i+1}
              post={p}
              user={users.find(u => u._id === p.userId)}
            />
          )}
          <ThreadHeader
            t={t}
            board={board}
            thread={thread}
            dir="column-reverse"
          />
        </div>
      </div>
    </div>
  )
}