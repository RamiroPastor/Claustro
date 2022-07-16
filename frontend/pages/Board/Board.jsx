import React, { useState } from "react"
import { useTranslation } from "next-i18next"

import { BoardHeader   } from "./BoardHeader/BoardHeader"
import { NewBoardModal } from "./NewBoardModal/NewBoardModal"


export function Board(props) {

  const t = useTranslation("common").t;

  const [modalActive_newForum, setModalActive_newForum ] = useState(false);
  const [boards, setBoards] = useState([])


  return(
    <div className="Board">
      <div className="Board__inner">
        <NewBoardModal
          t={t}
          isActive={modalActive_newForum}
          setActive={setModalActive_newForum}
        />
        <BoardHeader 
          t={t}
          setModalActive_newForum={setModalActive_newForum}
        />
        { (boards.length === 0)
        ? <div className="Board__inactive">
            <h3>{t("noActiveForums")}</h3>
          </div>
        : <h1>HOLA MUNDO</h1>
        }
      </div>
    </div>
  )
}