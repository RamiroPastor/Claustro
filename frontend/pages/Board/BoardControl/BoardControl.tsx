import React from "react"

import { archive  } from "frontend/assets/svg/archive"
import { maximize } from "frontend/assets/svg/maximize"
import { minimize } from "frontend/assets/svg/minimize"
import { pencil   } from "frontend/assets/svg/pencil"



export function BoardControl(
  props:
    { t                    : (s: string) => string
    , isBoardOpen          : boolean
    , switchBoardOpenClose : () => void
    , openArchiveModal     : () => void
    , openEditModal        : () => void
    }
  ) {

  const t = props.t;
  const isBoardOpen = props.isBoardOpen;
  const switchBoardOpenClose = props.switchBoardOpenClose;
  const openArchiveModal = props.openArchiveModal;
  const openEditModal = props.openEditModal;


  return(
    <div className="BoardControl">

      <button 
        className="BoardControl__button"
        type="button"
        title={isBoardOpen ? t("minimize") : t("maximize")}
        onClick={switchBoardOpenClose}
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
        onClick={openEditModal}
      >
        {pencil}
      </button>

      <button 
        className="BoardControl__button"
        type="button"
        title={t("archive")}
        onClick={openArchiveModal}
      >
        {archive}
      </button>

    </div>
  )
}