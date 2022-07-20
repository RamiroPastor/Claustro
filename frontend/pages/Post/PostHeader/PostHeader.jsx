import React from "react"



export function PostHeader(props) {

  const t = props.t;
  const post = props.post;


  return(
    <div className="PostHeader">
      <p>
        <em>{t("created")}:</em>
        <span>{post.createdAt.slice(0,10)}</span> 
        <span>{post.createdAt.slice(11,19)}</span>
      </p>
      <p>
        <em>{t("edited")}:</em>
        <span>-</span>
      </p>
    </div>
  )
}