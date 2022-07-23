import React from "react";

import { tick    } from "frontend/assets/svg/tick";
import { warning } from "frontend/assets/svg/warning";




export function Msg2(
  props:
    { isError : boolean
    , message : React.ReactNode
    }
  ) {

  const isError   = props.isError;
  const message   = props.message;


  return(
    <div className={`Msg2 ${isError ? "Msg2--error" : ""}`}>
      { isError
      ? warning
      : tick 
      }
      <p>
        {message}
      </p>
    </div>
  )
}