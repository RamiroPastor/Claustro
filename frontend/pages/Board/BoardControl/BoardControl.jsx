import React from "react"

import { archive  } from "frontend/assets/svg/archive"
import { maximize } from "frontend/assets/svg/maximize"
import { minimize } from "frontend/assets/svg/minimize"
import { pencil   } from "frontend/assets/svg/pencil"



export function BoardControl(props) {

  const t = props.t;
  const isBoardOpen = props.isBoardOpen;
  const setBoardOpen = props.setBoardOpen;
  const setModalActive_editBoard = props.setModalActive_editBoard;


  return(
    <div className="BoardControl">

      <button 
        className="BoardControl__button"
        type="button"
        title={isBoardOpen ? t("minimize") : t("maximize")}
        onClick={() => setBoardOpen(!isBoardOpen)}
      >
        { isBoardOpen
        ? minimize
        : maximize
        }
      </button>

      <button 
        className="BoardControl__button"
        type="button"
        title={t("edit")}
        onClick={() => setModalActive_editBoard(true)}
      >
        {pencil}
      </button>

      <button 
        className="BoardControl__button"
        type="button"
        title={t("archive")}
      >
        {archive}
      </button>

    </div>
  )
}