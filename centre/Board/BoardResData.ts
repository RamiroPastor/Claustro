import { HydratedDocument } from "mongoose"

import { IBoard } from "backend/Board/Board"



export interface BoardResData
  { _id           : string
  , createdByUser : string
  , title         : string
  , description   : string
  , languages     : string[]
  , priority      : number
  , archived      : boolean
  , createdAt     : string
  , updatedAt     : string
  }


export function toBoardResData(board : HydratedDocument<IBoard>) : BoardResData {

  return(
    { _id           : board._id.toString()
    , createdByUser : board.createdByUser.toString()
    , title         : board.title
    , description   : board.description
    , languages     : board.languages
    , priority      : board.priority
    , archived      : board.archived
    , createdAt     : board.createdAt.toJSON()
    , updatedAt     : board.updatedAt.toJSON()
    }
  )
}