import React from "react"

import { BoardHeader } from "./BoardHeader/BoardHeader"



export function Board(props) {

  const t = props.t;
  const board = props.board

  return(
    <div className="Board">
      <BoardHeader
        t={t}
        title={board.title}
        desc={board.description}
        langs={board.languages}
      />
      <div className="Board__body">
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
      </div>
    </div>
  )
}