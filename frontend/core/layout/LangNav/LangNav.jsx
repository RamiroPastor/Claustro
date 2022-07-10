import React from "react";
import Link from "next/link";
import { useRouter } from "next/router";

import { flagES } from "frontend/assets/svg/flagES";
import { flagGB } from "frontend/assets/svg/flagGB";



export function LangNav(props){

  const current = useRouter().pathname;

  return(
    <div className="LangNav">
      <div className="LangNav__inner">
        <Link href={current} locale="es">
          <a>
            { flagES }
          </a>
        </Link>
        <Link href={current} locale="en">
          <a>
            { flagGB }
          </a>
        </Link>
      </div>
    </div>
  )
}


