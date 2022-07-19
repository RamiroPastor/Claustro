import { NextApiRequest, NextApiResponse } from "next";

import { threadController } from "backend/Thread/controllerThread"
import { authController  } from "backend/User/controllerAuth"



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  const token       : String  = req.body.token;

  const threadData =
    { threadId: req.body.threadId
    , title: req.body.title
    , description: req.body.description
    , pinned: req.body.pinned
    , locked: req.body.locked
    };

  await authController.verifyUser(token).then(
    ({code, user}) => {
      threadController.updateThread(threadData).then(
        ({code}) => {
          res.status(code).json({ok: "ok"})
        }
      )
    },
    (err) => {
      console.error(err);
      res.status(err).json({ auth: false })
    }
  )
}