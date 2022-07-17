import React, { useState } from "react"
import { useTranslation } from "next-i18next"

import { Board          } from "frontend/pages/Board/Board"
import { Modal_NewBoard } from "frontend/pages/Board/Modal_NewBoard/Modal_NewBoard"
import { ForumHeader   } from "./ForumHeader/ForumHeader"


export function Forum(props) {

  const t = useTranslation("common").t;

  const [modalActive_newBoard, setModalActive_newBoard ] = useState(false);
  const boards = props.boardList


  return(
    <div className="Forum">
      <div className="Forum__inner">
        <Modal_NewBoard
          t={t}
          isActive={modalActive_newBoard}
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
            />
          )
        }
      </div>
    </div>
  )
}