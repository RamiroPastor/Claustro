export {}

/* 
import { NextApiRequest, NextApiResponse } from "next";

import { authController } from "backend/User/controllerAuth"



export default async function verify(req : NextApiRequest, res : NextApiResponse) {

  const token : string = req.body.token;

  await authController.verifyUser(token).then(
    ({code, user}) => {
      res.status(code).json({ auth: true, user: user})
    },
    (err) => {
      console.error(err);
      res.status(err).json({ auth: false })
    }
  )
}
 */