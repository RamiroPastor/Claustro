import { NextApiRequest, NextApiResponse } from "next";

import { postController } from "backend/Post/controllerPost"
import { threadController } from "backend/Thread/controllerThread"
import { authController  } from "backend/User/controllerAuth"
import { PostCreateData } from "centre/Post/PostCreateData";
import { ThreadCreateData } from "centre/Thread/ThreadCreateData";



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  const user = (await authController.verifyUser(req.body.token)).userResData;

  const threadCreateData : ThreadCreateData =
    { createdByUser : user._id
    , boardId       : req.body.boardId
    , title         : req.body.title
    , description   : req.body.description
    , pinned        : req.body.pinned
    , locked        : req.body.locked
    };

  const thread = (await threadController.registerThread(threadCreateData)).threadResData;

  const firstPost : PostCreateData =
    { userId: user._id
    , threadId: thread._id
    , body: req.body.post
    }
  
  const {code, postResData} = await postController.registerPost(firstPost);

  res.status(code).json({ ok: "ok" })
}