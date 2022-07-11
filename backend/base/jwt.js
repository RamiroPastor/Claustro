import jwt from "jsonwebtoken";


export function makeJWT(userID) {
  const token = jwt.sign(
    {id: userID},
    process.env.JWT_SECRET,
    { expiresIn: "7d"}
  )
  return token
}