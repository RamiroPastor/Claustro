import mongoose, { HydratedDocument } from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { Thread } from "backend/Thread/Thread"
import { User } from "backend/User/User"
import { PostCreateData } from "centre/Post/PostCreateData"
import { toPostResData  } from "centre/Post/PostResData"
import { IPost, Post } from "./Post"

export { postController }




const postController =
  { registerPost
  , listThreadPosts
  }


  
async function registerPost(data : PostCreateData) {

  let code : number = 500;

  await dbConn();

  const newPost : HydratedDocument<IPost> = new Post(
    { userId: data.userId
    , threadId: data.threadId
    , body: data.body
    }
  );

  const post = await newPost.save();
  await User.findOneAndUpdate({_id: data.userId}, {$inc: {posts: 1}});
  await Thread.findOneAndUpdate({_id: data.threadId}, {$inc: {postCount: 1}});
  const postResData = toPostResData(post)
  code = 200;
  
  return {code, postResData}
}



async function listThreadPosts(threadId : String){

  await dbConn();

  let postList : HydratedDocument<IPost>[] = []

  postList = 
    await Post.find()
                .where("threadId").equals(threadId)
  

  const postResList = postList.map(toPostResData)

  return postResList
}