import React, { useState } from "react"

import { BoardHeader } from "./BoardHeader/BoardHeader"



export function Board(props) {

  const t = props.t;
  const board = props.board

  const [isOpen, setOpen] = useState(true);


  return(
    <div className="Board">
      <BoardHeader
        t={t}
        board={board}
        isOpen={isOpen}
        setOpen={setOpen}
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