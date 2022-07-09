import React from "react";

import { logo } from "frontend/assets/svg/logo";



export function Logo(props) {

  return (
    <a className="Logo" href="/">
      { logo }
      <h1 className="Logo__text">Claustro</h1>
    </a>
  )
}