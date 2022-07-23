import mongoose, { HydratedDocument } from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { ThreadCreateData } from "centre/Thread/ThreadCreateData"
import { toThreadResData  } from "centre/Thread/ThreadResData"
import { ThreadUpdateData } from "centre/Thread/ThreadUpdateData"
import { IThread, Thread } from "./Thread"

export { threadController }




const threadController =
  { registerThread
  , updateThread
  , listThreads
  }



async function registerThread(data : ThreadCreateData) {

  let code : number = 500;

  await dbConn();

  const newThread : HydratedDocument<IThread> = new Thread(
    { createdByUser: data.createdByUser
    , boardId: data.boardId
    , title: data.title
    , description: data.description
    , pinned: data.pinned
    , locked: data.locked
    , lastActivity: 
      { userId: data.createdByUser
      , date: new Date()
      }
    }
  );

  const thread = await newThread.save();
  const threadResData = toThreadResData(thread);
  code = 200;
  
  return {code, threadResData}
}



async function updateThread(data : ThreadUpdateData) {

  let code : number = 500;

  await dbConn();

  const threadBeforeUpdate : HydratedDocument<IThread> | null =
    await Thread.findByIdAndUpdate(
      data.threadId,
      { title: data.title
      , description: data.description
      , pinned: data.pinned
      , locked: data.locked
      }
    );

  if (!threadBeforeUpdate) {
    code = 404;
    throw code;
  }
  
  code = 200;
  
  return {code}
}



async function listThreads(idList : string[]){

  await dbConn();

  let threadList : HydratedDocument<IThread>[]

  if (idList.length === 0) {
    threadList = await Thread.find();
  } else {
    threadList = 
      await Thread.find()
                  .where("_id").in(idList)
  }
  
  const resList = threadList.map(toThreadResData);

  return resList
}