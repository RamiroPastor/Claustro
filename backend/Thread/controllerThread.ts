import mongoose from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { Thread } from "./Thread"

export { threadController }




const threadController =
  { registerThread
  }



async function registerThread(data) {

  let code : number = 500;

  await dbConn();

  const newThread = new Thread(
    { createdByUser: data.userId
    , boardId: data.boardId
    , title: data.title
    , description: data.description
    , lastActivity: 
      { userId: data.userId
      , date: new Date()
      }
    }
  );

  const thread = await newThread.save();
  code = 200;
  
  return {code, thread}
}