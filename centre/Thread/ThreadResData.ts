import { HydratedDocument } from "mongoose"

import { IThread } from "backend/Thread/Thread"



export interface ThreadResData
  { _id           : string
  , createdByUser : string
  , boardId       : string
  , title         : string
  , description   : string
  , pinned        : number
  , locked        : boolean
  , postCount     : number
  , lastActivity  : 
    { userId : string
    , date   : string
    }
  , createdAt     : string
  , updatedAt     : string
  }


export function toThreadResData(thread : HydratedDocument<IThread>) : ThreadResData {

  return(
    { _id           : thread._id.toString()
    , createdByUser : thread.createdByUser.toString()
    , boardId       : thread.boardId.toString()
    , title         : thread.title
    , description   : thread.description
    , pinned        : thread.pinned
    , locked        : thread.locked
    , postCount     : thread.postCount
    , lastActivity  : 
      { userId : thread.lastActivity.userId.toString()
      , date   : thread.lastActivity.date.toString()
      }
    , createdAt     : thread.createdAt.toJSON()
    , updatedAt     : thread.updatedAt.toJSON()
    }
  )
}