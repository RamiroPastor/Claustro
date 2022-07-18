import React, { useRef, useState } from "react"

import { BoardHeader } from "./BoardHeader/BoardHeader"
import { Modal_ArchiveBoard } from "./Modal_ArchiveBoard/Modal_ArchiveBoard";
import { Modal_EditBoard } from "./Modal_EditBoard/Modal_EditBoard"



export function Board(props) {

  const t = props.t;
  const board = props.board

  const [isBoardOpen, setBoardOpen] = useState(true);
  const [isModalActive_editBoard, setModalActive_editBoard ] = useState(false);
  const [isModalActive_archiveBoard, setModalActive_archiveBoard ] = useState(false);
  const contentRef = useRef();

  const h = (contentRef && contentRef.current) ? contentRef.current.scrollHeight : 0;

  

  return(
    <div className="Board">
      <Modal_ArchiveBoard
        t={t}
        isActive={isModalActive_archiveBoard}
        setActive={setModalActive_archiveBoard}
      />
      <Modal_EditBoard
        t={t}
        isActive={isModalActive_editBoard}
        setActive={setModalActive_editBoard}
        board={board}
      />
      <BoardHeader
        t={t}
        board={board}
        isBoardOpen={isBoardOpen}
        setBoardOpen={setBoardOpen}
        setModalActive_editBoard={setModalActive_editBoard}
        setModalActive_archiveBoard={setModalActive_archiveBoard}
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