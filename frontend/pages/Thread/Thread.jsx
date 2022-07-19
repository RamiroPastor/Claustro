import React from "react"
import { useTranslation } from "next-i18next"



export function Thread(props) {

  const thread = props.thread;
  const posts  = props.postList;
  const users  = props.userList

  const t = useTranslation("common").t;



  return(
    <div className="Thread">
      <div className="Thread__inner">
        <p>{thread.title}</p>
        {posts.map((p,i) =>
          <div key={i}>
            {p.body}
          </div> 
        )}
        <h3>{users[0].name}</h3>
      </div>
    </div>
  )
}