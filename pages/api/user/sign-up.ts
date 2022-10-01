import { NextApiRequest, NextApiResponse } from "next";

import { authController } from "backend/User/controllerAuth"
import { SignUpData } from "centre/User/SignUpData";



export default async function handler(req : NextApiRequest, res : NextApiResponse) {
  
  const signUpData : SignUpData =
    { name     : req.body.name
    , email    : req.body.email
    , password : req.body.password
    }
  

  await authController.registerUser(signUpData).then(
    ({code, token, userResData}) => {
      res.status(code).json({ token, userResData })
    },
    (err) => {
      console.error(err);
      res.status(err).json({ auth: false })
    }
  )
}