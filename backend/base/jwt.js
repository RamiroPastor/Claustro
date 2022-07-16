import jsonwebtoken from "jsonwebtoken";



export const jwt = 
  { make: makeJWT
  , read: readJWT
  }


function makeJWT(userID) {
  const token = jsonwebtoken.sign(
    {id: userID},
    process.env.JWT_SECRET,
    { expiresIn: "7d"}
  )
  return token
}


function readJWT(token) {
  const userId = jsonwebtoken.verify(token, process.env.JWT_SECRET).id;
  return userId
}