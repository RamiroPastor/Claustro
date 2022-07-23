import React from "react"

import { PostResData } from "centre/Post/PostResData";



export function PostHeader(
  props:
    { t     : (s: string) => string
    , index : number
    , post  : PostResData
    }
  ) {

  const t = props.t;
  const index = props.index;
  const post = props.post;


  return(
    <div className="PostHeader">
      <p>
        <em>#</em>
        <span>{index}</span>
      </p>
      <p>
        <em>{t("created")}:</em>
        <span>{post.createdAt.slice( 0,10)}</span> 
        <span>{post.createdAt.slice(11,19)}</span>
      </p>
      <p>
        <em>{t("edited")}:</em>
        <span>-</span>
      </p>
    </div>
  )
}