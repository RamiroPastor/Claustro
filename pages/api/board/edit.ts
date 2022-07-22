import { NextApiRequest, NextApiResponse } from "next";

import { authController  } from "backend/User/controllerAuth"
import { boardController } from "backend/Board/controllerBoard"
import { BoardUpdateData } from "centre/Board/BoardUpdateData"



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  await authController.verifyUser(req.body.token);

  const boardUpdateData : BoardUpdateData =
    { boardId     : req.body.boardId
    , title       : req.body.title
    , description : req.body.description
    , languages   : req.body.lang
    }

  const {code} = await boardController.updateBoard(boardUpdateData);
  res.status(code).json({ok: "ok"})
}