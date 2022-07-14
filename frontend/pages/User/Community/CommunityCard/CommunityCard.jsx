import React from "react"
import Image from "next/image"


export function CommunityCard(props) {

  const user = props.user;


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
        <p>{user.email}</p>
        <p>{user.posts}</p>
        <p>{user.createdAt}</p>
      </div>
    </div>
  )
}