

import { dbConn } from "backend/base/dbConn"
import { User } from "./User"

export { communityController }




const communityController =
  { listUsers
  }





async function listUsers(idList : String[]){

  await dbConn();

  let userList = []

  if (idList.length === 0) {
    userList = await User.find();
  } else {
    userList = 
      await User.find()
                .where("_id").in(idList)
  }
  
  return userList.map(x => JSON.stringify(x))
}