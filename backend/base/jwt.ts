import jsonwebtoken, { Secret, JwtPayload } from "jsonwebtoken";



export const jwt = 
  { make: makeJWT
  , read: readJWT
  }


const jwtSecret : Secret = 
  process.env.JWT_SECRET ? process.env.JWT_SECRET : "secreto";


function makeJWT(userID : string) : string {
  const token = jsonwebtoken.sign(
    {id: userID},
    jwtSecret,
    { expiresIn: "7d"}
  )
  return token
}


function readJWT(token : string) : string | JwtPayload {
  const userId = jsonwebtoken.verify(token, jwtSecret);
  return userId
}