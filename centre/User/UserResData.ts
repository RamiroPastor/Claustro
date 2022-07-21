import { HydratedDocument } from "mongoose"

import { IUser } from "backend/User/User"



export interface UserResData 
  { _id     : string
  , name    : string
  , email   : string
  , posts   : number
  , picture : string
  , createdAt : string
  , updatedAt : string
  }


export function toUserResData(user : HydratedDocument<IUser>) {

  return(
    { _id     : user._id.toString()
    , name    : user.name
    , email   : user.email
    , posts   : user.posts
    , picture : user.picture
    , createdAt : user.createdAt.toJSON()
    , updatedAt : user.updatedAt.toJSON()
    } 
  )
}
    