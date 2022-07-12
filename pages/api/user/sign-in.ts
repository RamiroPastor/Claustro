import { NextApiRequest, NextApiResponse } from "next";

import { authController } from "backend/User/controllerAuth"



export default async function signIn(req : NextApiRequest, res : NextApiResponse) {

  const uEmail : string = req.body.email;
  const uPass  : string = req.body.password;

  await authController.logInUser(uEmail, uPass).then(
    ({code, token, name}) => {
      res.status(code).json({ auth: true, token: token, name: name })
    },
    (err) => {
      console.error(err);
      res.status(err).json({ auth: false })
    }
  )
}