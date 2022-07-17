import React from "react"

import { archive  } from "frontend/assets/svg/archive"
import { maximize } from "frontend/assets/svg/maximize"
import { minimize } from "frontend/assets/svg/minimize"
import { pencil   } from "frontend/assets/svg/pencil"



export function BoardControl(props) {

  const t = props.t;
  const isOpen = props.isOpen;
  const setOpen = props.setOpen;


  return(
    <div className="BoardControl">

      <button 
        className="BoardControl__button"
        type="button"
        title={isOpen ? t("minimize") : t("maximize")}
        onClick={() => setOpen(!isOpen)}
      >
        { isOpen
        ? minimize
        : maximize
        }
      </button>

      <button 
        className="BoardControl__button"
        type="button"
        title={t("edit")}
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