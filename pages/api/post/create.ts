import { NextApiRequest, NextApiResponse } from "next";

import { postController } from "backend/Post/controllerPost"
import { authController } from "backend/User/controllerAuth"



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  const token    : String   = req.body.token;
  const threadId : String   = req.body.threadId;
  const body     : String   = req.body.post;

  await authController.verifyUser(token).then(
    ({code, user}) => {
      postController.registerPost(
        { userId: user._id
        , threadId: threadId
        , body: body
        }
      ).then(
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