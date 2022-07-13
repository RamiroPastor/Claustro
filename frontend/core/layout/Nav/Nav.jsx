import React, { useContext } from "react"
import Link from "next/link";
import { useTranslation } from "next-i18next"

import { document } from "frontend/assets/svg/document"
import { key      } from "frontend/assets/svg/key"
import { people   } from "frontend/assets/svg/people"
import { AuthContext  } from "frontend/core/contexts/AuthContext"



export function Nav(props) {

  const t = useTranslation("common").t;

  const authContext = useContext(AuthContext);



  return(
    (authContext.isUserAuthenticated()) &&
    <nav className="Nav">

      <Link href="/">
        <a className="Nav__link">
          {document}
          <span>{t("forum")}</span>
        </a>
      </Link>

      <Link href="/user/list">
        <a className="Nav__link">
          {people}
          <span>{t("users")}</span>
        </a>
      </Link>

      <Link href="/">
        <a className="Nav__link" onClick={authContext.kickUser}>
          {key}
          <span>{t("exit")}</span>
        </a>
      </Link>

    </nav>
  )
}