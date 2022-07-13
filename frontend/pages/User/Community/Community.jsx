import React, { useState } from "react"
import { useTranslation } from "next-i18next"

import { CommunityHeader } from "./CommunityHeader/CommunityHeader"



export function Community(props) {

  const t = useTranslation("common").t;

  const [ viewPermissions, setViewPermissions ] = useState(false);
  const switchViewPermissions = () => setViewPermissions(!viewPermissions);



  return(
    <div className="Community">
      <div className="Community__inner">
        <CommunityHeader
          t={t}
          viewPermissions={viewPermissions}
          switchView={switchViewPermissions}
        />
        { viewPermissions
        ? <div>{t("permissionsToBeImplemented")}</div>
        : <div>LISTA DE USUARIOS</div>
        }
      </div>
    </div>
  )
}