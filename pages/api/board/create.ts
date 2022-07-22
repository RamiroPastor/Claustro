import { NextApiRequest, NextApiResponse } from "next";

import { authController  } from "backend/User/controllerAuth"
import { boardController } from "backend/Board/controllerBoard"
import { BoardCreateData } from "centre/Board/BoardCreateData"



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  const user = (await authController.verifyUser(req.body.token)).userResData;

  const boardCreateData : BoardCreateData =
    { createdByUser : user._id
    , title         : req.body.title
    , description   : req.body.description
    , languages     : req.body.lang
    }

  const {code, boardResData} = await boardController.registerBoard(boardCreateData);

  res.status(code).json(boardResData)
}