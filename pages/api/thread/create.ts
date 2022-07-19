import { NextApiRequest, NextApiResponse } from "next";

import { postController } from "backend/Post/controllerPost"
import { threadController } from "backend/Thread/controllerThread"
import { authController  } from "backend/User/controllerAuth"



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  const token       : String  = req.body.token;
  const boardId     : String  = req.body.boardId;
  const title       : String  = req.body.title;
  const description : String  = req.body.description;
  const pinned      : Number  = req.body.pinned;
  const locked      : Boolean = req.body.locked;
  const firstPost   : String  = req.body.post;

  await authController.verifyUser(token).then(
    ({code, user}) => {
      threadController.registerThread(
        { userId: user._id
        , boardId: boardId
        , title: title
        , description: description
        , pinned: pinned
        , locked: locked
        }
      ).then(
        ({code, thread}) => {
          postController.registerPost(
            { userId: user._id
            , threadId: thread._id
            , body: firstPost
            }
          ).then(
            ({code, post}) => {
              res.status(code).json({ ok: "ok" })
            }
          )
        }
      )
    },
    (err) => {
      console.error(err);
      res.status(err).json({ auth: false })
    }
  )
}