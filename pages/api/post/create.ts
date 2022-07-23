import { NextApiRequest, NextApiResponse } from "next";

import { postController } from "backend/Post/controllerPost"
import { authController } from "backend/User/controllerAuth"
import { PostCreateData } from "centre/Post/PostCreateData"



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  

  const user = (await authController.verifyUser(req.body.token)).userResData;

  const postCreateData : PostCreateData =
    { userId   : user._id
    , threadId : req.body.threadId
    , body     : req.body.post
    }

  const {code, postResData} = await postController.registerPost(postCreateData);


  res.status(code).json(postResData)
 

}