import mongoose from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { Thread } from "./Thread"

export { threadController }




const threadController =
  { registerThread
  , updateThread
  , listThreads
  }



async function registerThread(data) {

  let code : number = 500;

  await dbConn();

  const newThread = new Thread(
    { createdByUser: data.userId
    , boardId: data.boardId
    , title: data.title
    , description: data.description
    , pinned: data.pinned
    , locked: data.locked
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



async function updateThread(threadData) {

  let code : number = 500;

  await dbConn();

  const threadBeforeUpdate = await Thread.findByIdAndUpdate(
    threadData.threadId,
    { title: threadData.title
    , description: threadData.description
    , pinned: threadData.pinned
    , locked: threadData.locked
    }
  );

  if (!threadBeforeUpdate) {
    code = 404;
    throw code;
  }
  code = 200;
  
  return {code}
}



async function listThreads(idList : String[]){

  await dbConn();

  let boardList = []

  if (idList.length === 0) {
    boardList = await Thread.find();
  } else {
    boardList = 
      await Thread.find()
                  .where("_id").in(idList)
  }
  
  return boardList.map(x => JSON.stringify(x))
}