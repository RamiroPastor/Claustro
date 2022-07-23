import React from "react"
import Image from "next/image"

import { UserResData } from "centre/User/UserResData"
import { carnet   } from "frontend/assets/svg/carnet"
import { envelope } from "frontend/assets/svg/envelope"
import { pencil   } from "frontend/assets/svg/pencil"
import { onlyDate } from "frontend/base/js/onlyDate"



export function PostAuthor(
  props:
    { t    : (s: string) => string
    , user : UserResData
    }
  ) {

  const t = props.t;
  const user = props.user;


  const infoLine = (infoType : string, icon : React.ReactElement, data : React.ReactNode) =>
    <div className="PostAuthor__infoLine" title={infoType}>
      {icon}
      <span>{data}</span>
    </div>


  return(
    <div className="PostAuthor">
      <div className="PostAuthor__image">
        <Image 
          src={user.picture} 
          alt="" 
          height={120} 
          width={120}
          layout="fixed"
        />
      </div>
      <div className="PostAuthor__infoWrap">
        <h3 className="PostAuthor__name">
          {user.name}
        </h3>
        <div className="PostAuthor__info">
          {infoLine(t("email")   , envelope, user.email)}
          {infoLine(t("joinedAt"), carnet  , onlyDate(user.createdAt))}
          {infoLine(t("posts")   , pencil  , user.posts)}
        </div>
      </div>
    </div>
  )
}