import { NextApiRequest, NextApiResponse } from "next";

import { threadController } from "backend/Thread/controllerThread"
import { authController  } from "backend/User/controllerAuth"
import { ThreadUpdateData } from "centre/Thread/ThreadUpdateData"



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  await authController.verifyUser(req.body.token)

  const threadUpdateData : ThreadUpdateData =
    { threadId: req.body.threadId
    , title: req.body.title
    , description: req.body.description
    , pinned: req.body.pinned
    , locked: req.body.locked
    };

  const {code} = await threadController.updateThread(threadUpdateData);

  res.status(code).json({ok : "ok"})
}