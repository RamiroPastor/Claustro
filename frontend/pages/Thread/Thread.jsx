import React, { useEffect, useRef, useState } from "react"
import { useTranslation } from "next-i18next"

import { Post } from "frontend/pages/Post/Post"
import { PostCreate } from "frontend/pages/Post/PostCreate/PostCreate"
import { ThreadHeader } from "./ThreadHeader/ThreadHeader"
import { ThreadPaginator } from "./ThreadHeader/ThreadPaginator/ThreadPaginator"


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



  // PAGINATOR
  const items = posts;
  const itemsPerPage = 10;
  const totalItemsCount = items.length;
  const [currentItems, setCurrentItems] = useState(items.slice(0, itemsPerPage));
  const [activePage  , setActivePage  ] = useState(1);


  const handlePageChange = n => {
    console.log(n)
    if (n != activePage) {
      setCurrentItems(items.slice((n - 1) * itemsPerPage, n * itemsPerPage))
      setActivePage(n);
    }
  };


  const Paginator = () =>
    <ThreadPaginator
      activePage={activePage}
      itemsPerPage={itemsPerPage}
      totalItemsCount={totalItemsCount}
      handlePageChange={handlePageChange}
    />


  const Header = ({dir}) => 
    <ThreadHeader
      t={t}
      board={board}
      thread={thread}
      dir={dir}
      openReplyBox={openReplyBox}
      Paginator={Paginator}
    />




  return(
    <div className="Thread">
      <div className="Thread__background">
        <div className="Thread__inner">
          <Header
            dir="column"
          />
          {(currentItems ? currentItems : items).map((p,i) =>
            <Post
              key={i}
              t={t}
              index={(activePage - 1) * itemsPerPage + i + 1}
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
          <Header
            dir="column-reverse"
          />
        </div>
      </div>
    </div>
  )
}