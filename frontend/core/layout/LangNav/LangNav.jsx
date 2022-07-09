import React from "react";
import Link from "next/link";

import { flagES } from "frontend/assets/svg/flagES";
import { flagGB } from "frontend/assets/svg/flagGB";



export function LangNav(props){

  return(
    <div className="LangNav">
      <div className="LangNav__inner">
        <Link href="/" locale="es">
          <a>
            { flagES }
          </a>
        </Link>
        <Link href="/" locale="en">
          <a>
            { flagGB }
          </a>
        </Link>
      </div>
    </div>
  )
}


