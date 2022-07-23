import React from "react";
import Link from "next/link"

import { logo } from "frontend/assets/svg/logo";



export function Logo() {

  return (
    <Link href="/">
      <a className="Logo">
        { logo }
        <h1 className="Logo__text">Claustro</h1>
      </a>
    </Link>
  )
}