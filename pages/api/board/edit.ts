import { NextApiRequest, NextApiResponse } from "next";

import { authController  } from "backend/User/controllerAuth"
import { boardController } from "backend/Board/controllerBoard"



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  const token = req.body.token;
  const boardData =
    { boardId: req.body.boardId
    , title: req.body.title
    , description: req.body.description
    , lang: req.body.lang
    };

  await authController.verifyUser(token).then(
    ({code, user}) => {
      boardController.updateBoard(boardData).then(
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