import { NextApiRequest, NextApiResponse } from "next";


export default function signIn(req : NextApiRequest, res : NextApiResponse) {
  console.log(req.body);
  res.status(200).json({ name: 'John Doe' })
}