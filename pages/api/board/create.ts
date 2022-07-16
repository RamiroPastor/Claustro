import { NextApiRequest, NextApiResponse } from "next";

import { authController  } from "backend/User/controllerAuth"
import { boardController } from "backend/Board/controllerBoard"



export default async function signUp(req : NextApiRequest, res : NextApiResponse) {
  
  const token : String   = req.body.token;
  const title : String   = req.body.title;
  const desc  : String   = req.body.description;
  const langs : [String] = req.body.lang;

  await authController.verifyUser(token).then(
    ({code, user}) => {
      boardController.registerBoard(user._id, title, desc, langs).then(
        ({code, board}) => {
          res.status(code).json({ board: board })
        }
      )
    },
    (err) => {
      console.error(err);
      res.status(err).json({ auth: false })
    }
  )
}