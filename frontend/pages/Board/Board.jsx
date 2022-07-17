import React, { useRef, useState } from "react"

import { BoardHeader } from "./BoardHeader/BoardHeader"



export function Board(props) {

  const t = props.t;
  const board = props.board

  const [isBoardOpen, setBoardOpen] = useState(true);
  const contentRef = useRef();

  const h = (contentRef && contentRef.current) ? contentRef.current.scrollHeight : 0;

  

  return(
    <div className="Board">
      <BoardHeader
        t={t}
        board={board}
        isBoardOpen={isBoardOpen}
        setBoardOpen={setBoardOpen}
      />
      <div 
        ref={contentRef}
        className="Board__body"
        style={ !isBoardOpen ? {} : {maxHeight: h}}
      >
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
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
        <p>Lorem ipsum dolor sit amet</p>
      </div>
    </div>
  )
}