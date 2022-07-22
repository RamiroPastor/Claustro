import React, { useRef, useState } from "react"
import { useTranslation } from "next-i18next"

import { BoardResData } from "centre/Board/BoardResData"
import { ThreadResData } from "centre/Thread/ThreadResData"
import { UserResData } from "centre/User/UserResData"
import { ThreadMiniature } from "frontend/pages/Thread/ThreadMiniature/ThreadMiniature";
import { BoardFooter } from "./BoardFooter/BoardFooter"
import { BoardHeader } from "./BoardHeader/BoardHeader"
import { Modal_ArchiveBoard } from "./Modal_ArchiveBoard/Modal_ArchiveBoard";
import { Modal_EditBoard } from "./Modal_EditBoard/Modal_EditBoard"



export function Board(
  props:
    { board    : BoardResData
    , threads  : ThreadResData[]
    , userList : UserResData[] 
    }
  ) {

  const board = props.board;
  const threads = props.threads;
  const userList = props.userList;

  const t = useTranslation("common").t;
  const [isBoardOpen, setBoardOpen] = useState(true);
  const switchBoardOpenClose = () => {setBoardOpen(!isBoardOpen)}
  const [isModalActive_editBoard, setModalActive_editBoard ] = useState(false);
  const openEditModal  = () => {setModalActive_editBoard(true)}
  const closeEditModal = () => {setModalActive_editBoard(false)}
  const [isModalActive_archiveBoard, setModalActive_archiveBoard ] = useState(false);
  const openArchiveModal  = () => {setModalActive_archiveBoard(true)}
  const closeArchiveModal = () => {setModalActive_archiveBoard(false)}


  const bodyRef = useRef<HTMLDivElement>(null);

  const h = (bodyRef && bodyRef.current) ? bodyRef.current.scrollHeight : 0;



  return(
    <div className="Board" id={board._id}>
      <Modal_ArchiveBoard
        t={t}
        isActive={isModalActive_archiveBoard}
        closeModal={closeArchiveModal}
      />
      <Modal_EditBoard
        t={t}
        isActive={isModalActive_editBoard}
        closeModal={closeEditModal}
        board={board}
      />
      <BoardHeader
        t={t}
        board={board}
        isBoardOpen={isBoardOpen}
        switchBoardOpenClose={switchBoardOpenClose}
        openArchiveModal={openArchiveModal}
        openEditModal={openEditModal}
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