import React, { useContext } from "react"
import Link from "next/link";
import { useTranslation } from "next-i18next"

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
          <span>{t("forum")}</span>
        </a>
      </Link>

      <Link href="/user/list">
        <a className="Nav__link">
          <span>{t("users")}</span>
        </a>
      </Link>

      <Link href="/">
        <a className="Nav__link" onClick={() => kickUser(setUser)}>
          <span>{t("exit")}</span>
        </a>
      </Link>

    </nav>
  )
}