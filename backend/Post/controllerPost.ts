import mongoose from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { User } from "backend/User/User" 
import { Post } from "./Post"

export { postController }




const postController =
  { registerPost
  , listThreadPosts
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
  await User.findOneAndUpdate({_id: data.userId}, {$inc: {posts: 1}});
  code = 200;
  
  return {code, post}
}


async function listThreadPosts(threadId : String){

  await dbConn();

  let postList = []

  postList = 
    await Post.find()
                .where("threadId").equals(threadId)
  
  return postList.map(x => JSON.stringify(x))
}