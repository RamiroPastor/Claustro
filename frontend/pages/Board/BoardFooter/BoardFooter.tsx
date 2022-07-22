import React from "react"
import Link from "next/link"

import { BoardResData } from "centre/Board/BoardResData";



export function BoardFooter(
  props:
    { t : (s: string) => string
    , board: BoardResData
    }
  ) {

  const t = props.t;
  const board = props.board;



  return(
    <div className="BoardFooter">
      <Link href={`/thread/create?board_id=${board._id}&board_title=${board.title}`}>
        <a className="BoardFooter__newThreadButton">
          <em>🧵</em>
          <span>{t("newThread")}</span>
          <em>🧵</em>
        </a>
      </Link>
    </div>
  )
}