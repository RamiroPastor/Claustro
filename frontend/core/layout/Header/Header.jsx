import React from "react";

import { Logo } from "frontend/core/layout/Logo/Logo";
// import { Nav  } from "core/layout/Nav/Nav"; 



export function Header(props) {


  return(
    <header className="Header">
      <div className="Header__inner">
        <Logo/>
      </div>
    </header>
  )
}