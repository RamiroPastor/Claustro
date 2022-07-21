import { NextApiRequest, NextApiResponse } from "next";

import { authController } from "backend/User/controllerAuth"
import { SignInData } from "centre/User/SignInData";



export default async function handler(req : NextApiRequest, res : NextApiResponse) {

  const signInData : SignInData =
    { email:    req.body.email
    , password: req.body.password
    }

  await authController.logInUser(signInData).then(
    ({code, token, userResData}) => {
      res.status(code).json({ token, userResData })
    },
    (err) => {
      console.error(err);
      res.status(err).json({ auth: false })
    }
  )
}