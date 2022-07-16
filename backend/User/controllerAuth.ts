import bcrypt from "bcrypt";

import { dbConn } from "backend/base/dbConn"
import { jwt    } from "backend/base/jwt"
import { randomImg } from "backend/base/randomImageURL"
import { config } from "centre/config/config"
import { User } from "./User"

export { authController }




const authController =
  { registerUser
  , logInUser
  , verifyUser
  }



async function registerUser(uName : String, uEmail : String, uPass : String) {

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
  const token = jwt.make(user._id)
  code = 200;

  return {code, token}
}





async function logInUser(uEmail : String, uPass : String) {

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

  const token = jwt.make(user._id)
  code = 200;
  
  return {code, token, name: user.name}
}



async function verifyUser(token : String) {

  let code = 500;
  const userId = jwt.read(token);

  await dbConn();
  const user = await User.findOne({_id: userId});
    if (!user) {
      code = 401;
      throw code
    }

  return {code, user}
}