import React from "react"
import Link from "next/link"



export function ThreadMiniature(props) {

  const thread = props.thread;
  const userList = userList;



  return(
    <div className="ThreadMiniature">
      <div className="ThreadMiniature__state">
        iconos
      </div>
      <div className="ThreadMiniature__info">
        <Link href={`/thread/${thread._id}`}>
          <a className="ThreadMiniature__link">
            {thread.title}
          </a>
        </Link>
        <p className="ThreadMiniature__description">
          {thread.description}
        </p>
      </div>
    </div>
  )
}