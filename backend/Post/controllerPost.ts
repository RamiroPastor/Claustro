import mongoose from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { Post } from "./Post"

export { postController }




const postController =
  { registerPost
  }


async function registerPost(data) {

  let code : number = 500;

  await dbConn();

  const newPost = new Post(
    { userId: data.userId
    , threadId: data.threadId
    , body: data.body
    }
  );

  const post = await newPost.save();
  code = 200;
  
  return {code, post}
}