import React from "react"

import { langToFlag } from "frontend/base/js/langToFlag";
import { BoardControl } from "../BoardControl/BoardControl";



export function BoardHeader(props) {

  const t = props.t;
  const board = props.board;
  const isBoardOpen = props.isBoardOpen;
  const setBoardOpen = props.setBoardOpen;
  const setModalActive_editBoard = props.setModalActive_editBoard;
  const setModalActive_archiveBoard = props.setModalActive_archiveBoard;



  return(
    <div className="BoardHeader">

      <div className="BoardHeader__head">
        <h3>
          {board.title}
        </h3>
        <BoardControl
          t={t}
          isBoardOpen={isBoardOpen}
          setBoardOpen={setBoardOpen}
          setModalActive_editBoard={setModalActive_editBoard}
          setModalActive_archiveBoard={setModalActive_archiveBoard}
        />
      </div>

      <div className="BoardHeader__description">
        <p>
          {board.description}
        </p>
      </div>

      <div className="BoardHeader__langList">
        { board.languages.map((lang, i) =>
          <span key={i} className="BoardHeader__lang">
            {langToFlag(lang)}
            <em>{t(lang)}</em>
          </span>
        )}
      </div>

    </div>
  )
}