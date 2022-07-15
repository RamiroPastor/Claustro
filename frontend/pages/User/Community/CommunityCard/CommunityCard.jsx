import React from "react"
import Image from "next/image"
import { useTranslation } from "next-i18next"

import { carnet   } from "frontend/assets/svg/carnet"
import { envelope } from "frontend/assets/svg/envelope"
import { pencil   } from "frontend/assets/svg/pencil"
import { onlyDate } from "frontend/base/js/onlyDate"



export function CommunityCard(props) {

  const user = props.user;

  const t = useTranslation("common").t;

  const infoLine = (infoType, icon, data) =>
    <div className="CommunityCard__infoLine" title={infoType}>
      {icon}
      <span>{data}</span>
    </div>


  return(
    <div className="CommunityCard">
      <Image 
        src={user.picture} 
        alt="" 
        height={200} 
        width={200}
        layout="fixed"
      />
      <h3 className="CommunityCard__name">
        {user.name}
      </h3>
      <div className="CommunityCard__info">
        {infoLine(t("email")   , envelope, user.email)}
        {infoLine(t("joinedAt"), carnet  , onlyDate(user.createdAt))}
        {infoLine(t("posts")   , pencil  , user.posts)}
      </div>
    </div>
  )
}