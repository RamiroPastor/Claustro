import React, { useState } from "react"
import { useTranslation } from "next-i18next"

import { UserResData } from "centre/User/UserResData"
import { CommunityCard   } from "./CommunityCard/CommunityCard"
import { CommunityHeader } from "./CommunityHeader/CommunityHeader"



export function Community(props : {userList : UserResData[]}) {

  const userList = props.userList;

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
        : <div className="Community__gallery">
            {userList.map((u,i) => <CommunityCard key={i} user={u}/>)}
          </div>
        }
      </div>
    </div>
  )
}