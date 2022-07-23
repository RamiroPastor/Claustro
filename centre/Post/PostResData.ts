import { HydratedDocument } from "mongoose"

import { IPost } from "backend/Post/Post"



export interface PostResData
  { _id       : string
  , userId    : string
  , threadId  : string
  , body      : string
  , createdAt : string
  , updatedAt : string
  }



export function toPostResData(post : HydratedDocument<IPost>) : PostResData {

  return(
    { _id       : post._id.toString()
    , userId    : post.userId.toString()
    , threadId  : post.threadId.toString()
    , body      : post.body
    , createdAt : post.createdAt.toJSON()
    , updatedAt : post.updatedAt.toJSON()
    }
  )
}