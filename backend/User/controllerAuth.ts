import bcrypt from "bcrypt";

import { dbConn  } from "backend/base/dbConn"
import { makeJWT } from "backend/base/jwt"
import { config } from "centre/config/config"
import { User } from "./User"

export { authController }




const authController =
  { registerUser
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
  const newUser = new User(
    { name: uName
    , email: uEmail
    , password: hash
    }
  );

  const user = await newUser.save();
  const token = makeJWT(user._id)

  code = 200;
  
  return {code, token}
}
