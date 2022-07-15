import React from "react"
import { useTranslation } from "next-i18next"

import { BoardHeader } from "./BoardHeader/BoardHeader"


export function Board(props) {

  const t = useTranslation("common").t;


  return(
    <div className="Board">
      <div className="Board__inner">
        <BoardHeader t={t}/>
      </div>
    </div>
  )
}