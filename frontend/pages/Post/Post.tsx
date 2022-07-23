import React from "react"
import MarkdownIt from "markdown-it"
import MarkdownItVideo from "markdown-it-video"

import { PostResData } from "centre/Post/PostResData"
import { UserResData } from "centre/User/UserResData"
import { PostAuthor } from "./PostAuthor/PostAuthor"
import { PostHeader } from "./PostHeader/PostHeader"



export function Post(
  props:
    { t     : (s: string) => string
    , index : number
    , post  : PostResData
    , user  : UserResData | undefined
    }
  ){

  const t = props.t;
  const index = props.index;
  const post = props.post;
  const user = props.user;


  const mdParser = new MarkdownIt(
    { linkify: true
    }
  ).use(MarkdownItVideo, 
    { youtube: { width: 640, height: 390 }
    , vimeo: { width: 500, height: 281 }
    , vine: { width: 600, height: 600, embed: 'simple' }
    , prezi: { width: 550, height: 400 }
    })



  return(
    <div className="Post">
      { user &&
      <PostAuthor
        t={t}
        user={user}
      />
      }
      <div className="Post__inner">
        <PostHeader
          t={t}
          index={index}
          post={post}
        />
        <div 
          className="Post__body"
          dangerouslySetInnerHTML={{__html: mdParser.render(post.body)}}
        >
        </div>
      </div>
    </div>
  )
}