import React, { useState } from "react"
import { useTranslation } from "next-i18next"

import { NewBoardModal } from "frontend/pages/Board/NewBoardModal/NewBoardModal"
import { ForumHeader   } from "./ForumHeader/ForumHeader"


export function Forum(props) {

  const t = useTranslation("common").t;

  const [modalActive_newBoard, setModalActive_newBoard ] = useState(false);
  const [boards, setBoards] = useState([])


  return(
    <div className="Forum">
      <div className="Forum__inner">
        <NewBoardModal
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
        : <h1>HOLA MUNDO</h1>
        }
      </div>
    </div>
  )
}