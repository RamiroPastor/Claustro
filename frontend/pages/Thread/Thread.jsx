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
  const [currentItems, setCurrentItems] = useState(null);
  const [pageCount   , setPageCount   ] = useState(0);
  const [itemOffset  , setItemOffset  ] = useState(0);


  useEffect(() => {
    const endOffset = itemOffset + itemsPerPage;
    setCurrentItems(items.slice(itemOffset, endOffset));
    setPageCount(Math.ceil(items.length / itemsPerPage));
  }, [itemOffset, itemsPerPage, items]);

  const handlePaginatorClick = (event) => {
    console.log(event.selected);
    const newOffset = (event.selected * itemsPerPage);
    setItemOffset(newOffset);
  };


  const Paginator = () =>
    <ThreadPaginator
      handlePaginatorClick={handlePaginatorClick}
      pageCount={pageCount}
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
              index={itemOffset + i + 1}
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