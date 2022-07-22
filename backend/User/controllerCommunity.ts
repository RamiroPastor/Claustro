import { HydratedDocument } from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { toUserResData } from "centre/User/UserResData"
import { IUser, User } from "./User"

export { communityController }




const communityController =
  { listUsers
  }



async function listUsers(idList : string[]){

  await dbConn();

  let userList : HydratedDocument<IUser>[]

  if (idList.length === 0) {
    userList = await User.find();
  } else {
    userList = 
      await User.find()
                .where("_id").in(idList)
  }

  const resList = userList.map(toUserResData);
  
  return resList
}
