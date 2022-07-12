import React, { useContext } from "react"
import Link from "next/link";
import { useTranslation } from "next-i18next"

import { document } from "frontend/assets/svg/document"
import { key      } from "frontend/assets/svg/key"
import { people   } from "frontend/assets/svg/people"
import { kickUser } from "frontend/base/js/kickUser"
import { UserContext  } from "frontend/core/contexts/UserContext"



export function Nav(props) {

  const t = useTranslation("common").t;

  const {user, setUser} = useContext(UserContext);



  return(
    (user.jwt !== null) &&
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
        <a className="Nav__link" onClick={() => kickUser(setUser)}>
          {key}
          <span>{t("exit")}</span>
        </a>
      </Link>

    </nav>
  )
}