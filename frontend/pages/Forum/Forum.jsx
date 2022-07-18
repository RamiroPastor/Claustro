import React, { useState } from "react"
import { useTranslation } from "next-i18next"

import { Board          } from "frontend/pages/Board/Board"
import { Modal_NewBoard } from "frontend/pages/Board/Modal_NewBoard/Modal_NewBoard"
import { ForumHeader   } from "./ForumHeader/ForumHeader"


export function Forum(props) {

  const t = useTranslation("common").t;

  const [isModalActive_newBoard, setModalActive_newBoard ] = useState(false);
  const boards = props.boardList
  const threadList = props.threadList
  const userList = props.userList



  return(
    <div className="Forum">
      <div className="Forum__inner">
        <Modal_NewBoard
          t={t}
          isActive={isModalActive_newBoard}
          setActive={setModalActive_newBoard}
        />
        <ForumHeader 
          t={t}
          setModalActive_newBoard={setModalActive_newBoard}
        />
        { (boards.length === 0)
        ? <div className="Forum__inactive">
            <p>{t("noActiveForums")}</p>
          </div>
        : boards.map((b,i) =>
            <Board
              key={i}
              t={t}
              board={b}
              threads={threadList.filter(th => b._id === th.boardId)}
              userList={userList}
            />
          )
        }
      </div>
    </div>
  )
}