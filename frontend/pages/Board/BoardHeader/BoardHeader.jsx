import React from "react"

import { langToFlag } from "frontend/base/js/langToFlag";
import { BoardControl } from "../BoardControl/BoardControl";



export function BoardHeader(props) {

  const t = props.t;
  const board = props.board;
  const isOpen = props.isOpen;
  const setOpen = props.setOpen;


  return(
    <div className="BoardHeader">

      <div className="BoardHeader__head">
        <h3>
          {board.title}
        </h3>
        <BoardControl
          t={t}
          isOpen={isOpen}
          setOpen={setOpen}
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