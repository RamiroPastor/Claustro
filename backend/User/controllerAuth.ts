import bcrypt from "bcrypt";

import { dbConn    } from "backend/base/dbConn"
import { makeJWT   } from "backend/base/jwt"
import { randomImg } from "backend/base/randomImageURL"
import { config } from "centre/config/config"
import { User } from "./User"

export { authController }




const authController =
  { registerUser
  , logInUser
  }



async function registerUser(uName : string, uEmail : string, uPass : string) {

  let code : number = 500;

  await dbConn();

  const userAlreadyExists = await User.findOne({ email: uEmail });
  if (userAlreadyExists) {
    code = 409;
    throw code;
  }

  if 
    (  uPass.length < config.user.minLen_password
    || uPass.length > config.user.maxLen_password
    ) {
    code = 413;
    throw code;
  }

  const hash = await bcrypt.hash(uPass, 10);
  const imgURL = randomImg();
  const newUser = new User(
    { name: uName
    , email: uEmail
    , password: hash
    , posts: 0
    , picture: imgURL
    }
  );

  const user = await newUser.save();
  const token = makeJWT(user._id)

  code = 200;
  
  return {code, token}
}





async function logInUser(uEmail : string, uPass : string) {

  let code : number = 500;

  await dbConn();

  const user = await User.findOne({email: uEmail});
  if (!user) {
    code = 404;
    throw code;
  }

  const passwordIsValid = await bcrypt.compare(uPass, user.password);
  if (!passwordIsValid) {
    code = 401;
    throw code;
  }

  const token = makeJWT(user._id)

  code = 200;
  
  return {code, token, name: user.name}
}