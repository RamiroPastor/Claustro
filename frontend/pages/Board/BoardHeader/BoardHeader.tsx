import React from "react"

import { BoardResData } from "centre/Board/BoardResData";
import { langToFlag } from "frontend/base/js/langToFlag";
import { BoardControl } from "../BoardControl/BoardControl";



export function BoardHeader(
  props:
    { t                    : (s: string) => string
    , board                : BoardResData
    , isBoardOpen          : boolean
    , switchBoardOpenClose : () => void
    , openArchiveModal     : () => void
    , openEditModal        : () => void
    }
  ) {

  const t = props.t;
  const board = props.board;
  const isBoardOpen = props.isBoardOpen;
  const switchBoardOpenClose = props.switchBoardOpenClose;
  const openArchiveModal = props.openArchiveModal;
  const openEditModal = props.openEditModal;



  return(
    <div className="BoardHeader">

      <div className="BoardHeader__head">
        <h3>
          {board.title}
        </h3>
        <BoardControl
          t={t}
          isBoardOpen={isBoardOpen}
          switchBoardOpenClose={switchBoardOpenClose}
          openArchiveModal={openArchiveModal}
          openEditModal={openEditModal}
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