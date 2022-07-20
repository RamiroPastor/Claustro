import React, { useEffect, useRef, useState } from "react"
import { useTranslation } from "next-i18next"

import { Post } from "frontend/pages/Post/Post"
import { PostCreate } from "frontend/pages/Post/PostCreate/PostCreate"
import { ThreadHeader } from "./ThreadHeader/ThreadHeader"


export function Thread(props) {

  const board  = props.board;
  const thread = props.thread;
  const posts  = props.postList;
  const users  = props.userList

  const t = useTranslation("common").t;

  const replyBoxRef = useRef(null);
  const [isReplyBoxActive, setReplyBoxActive] = useState(0);

  const openReplyBox = () => setReplyBoxActive(isReplyBoxActive + 1)
  const closeReplyBox = () => setReplyBoxActive(0)
  

  useEffect(() => {
    replyBoxRef.current?.scrollIntoView({ behavior: "smooth", block: "center" });
  }, [isReplyBoxActive])




  return(
    <div className="Thread">
      <div className="Thread__background">
        <div className="Thread__inner">
          <ThreadHeader
            t={t}
            board={board}
            thread={thread}
            dir="column"
            openReplyBox={openReplyBox}
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
          { (isReplyBoxActive != 0) &&
          <PostCreate
            t={t}
            threadId={thread._id}
            replyBoxRef={replyBoxRef}
            closeReplyBox={closeReplyBox}
          />
          }
          <ThreadHeader
            t={t}
            board={board}
            thread={thread}
            dir="column-reverse"
            openReplyBox={openReplyBox}
          />
        </div>
      </div>
    </div>
  )
}