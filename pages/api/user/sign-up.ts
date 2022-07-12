import { NextApiRequest, NextApiResponse } from "next";

import { authController } from "backend/User/controllerAuth"



export default async function signUp(req : NextApiRequest, res : NextApiResponse) {
  
  const uName  : string = req.body.username;
  const uEmail : string = req.body.email;
  const uPass  : string = req.body.password;

  await authController.registerUser(uName, uEmail, uPass).then(
    ({code, token}) => {
      res.status(code).json({ auth: true, token: token })
    },
    (err) => {
      console.error(err);
      res.status(err).json({ auth: false })
    }
  )
}