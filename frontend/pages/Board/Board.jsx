import React, { useRef, useState } from "react"

import { ThreadMiniature } from "frontend/pages/Thread/ThreadMiniature/ThreadMiniature";
import { BoardFooter } from "./BoardFooter/BoardFooter"
import { BoardHeader } from "./BoardHeader/BoardHeader"
import { Modal_ArchiveBoard } from "./Modal_ArchiveBoard/Modal_ArchiveBoard";
import { Modal_EditBoard } from "./Modal_EditBoard/Modal_EditBoard"



export function Board(props) {

  const t = props.t;
  const board = props.board;
  const threads = props.threads;
  const userList = props.userList;

  const [isBoardOpen, setBoardOpen] = useState(true);
  const [isModalActive_editBoard, setModalActive_editBoard ] = useState(false);
  const [isModalActive_archiveBoard, setModalActive_archiveBoard ] = useState(false);
  const bodyRef = useRef();

  const h = (bodyRef && bodyRef.current) ? bodyRef.current.scrollHeight : 0;



  return(
    <div className="Board">
      <Modal_ArchiveBoard
        t={t}
        isActive={isModalActive_archiveBoard}
        setActive={setModalActive_archiveBoard}
      />
      <Modal_EditBoard
        t={t}
        isActive={isModalActive_editBoard}
        setActive={setModalActive_editBoard}
        board={board}
      />
      <BoardHeader
        t={t}
        board={board}
        isBoardOpen={isBoardOpen}
        setBoardOpen={setBoardOpen}
        setModalActive_editBoard={setModalActive_editBoard}
        setModalActive_archiveBoard={setModalActive_archiveBoard}
      />
      <div 
        ref={bodyRef}
        className="Board__body"
        style={ !isBoardOpen ? {} : {maxHeight: h}}
      >
        <div className="Board__content">
          { threads.length === 0
          ? 
          <p className="Board__contentEmpty">
            {t("noThreadsInThisBoard")}
          </p>
          : 
          threads.map((th, i) =>
            <ThreadMiniature
              key={i}
              t={t}
              thread={th}
              userList={userList}
            />
          )}
        </div>
        <BoardFooter
          t={t}
          board={board}
        />
      </div>
    </div>
  )
}