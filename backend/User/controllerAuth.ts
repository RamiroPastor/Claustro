import bcrypt from "bcrypt";
import { HydratedDocument } from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { jwt    } from "backend/base/jwt"
import { randomImg } from "backend/base/randomImageURL"
import { config } from "centre/config/config"
import { SignInData } from "centre/User/SignInData"
import { SignUpData } from "centre/User/SignUpData"
import { toUserResData } from "centre/User/UserResData"
import { IUser, User } from "./User"

export { authController }




const authController =
  { registerUser
  , logInUser
  , verifyUser
  }



async function registerUser(uData : SignUpData) {

  let code : number = 500;

  await dbConn();
  const userAlreadyExists : HydratedDocument<IUser> | null =
    await User.findOne({ email: uData.email});
  if (userAlreadyExists) {
    code = 409;
    throw code;
  }
  if 
    (  uData.password.length < config.user.minLen_password
    || uData.password.length > config.user.maxLen_password
    ) {
    code = 413;
    throw code;
  }

  const hash   = await bcrypt.hash(uData.password, 10);
  const imgURL = randomImg();
  const newUser : HydratedDocument<IUser> = new User(
    { name: uData.name
    , email: uData.email
    , password: hash
    , posts: 0
    , picture: imgURL
    }
  );

  const user = await newUser.save();
  const token = jwt.make(user._id.toString())
  const userResData = toUserResData(user);
  code = 200;
  
  return {code, token, userResData}
}





async function logInUser(uData : SignInData) {

  let code : number = 500;

  await dbConn();
  const user : HydratedDocument<IUser> | null = 
    await User.findOne({email: uData.email});
  if (!user) {
    code = 404;
    throw code;
  }
  const passwordIsValid = 
    await bcrypt.compare(uData.password, user.password);
  if (!passwordIsValid) {
    code = 401;
    throw code;
  }

  const token = jwt.make(user._id.toString())
  const userResData = toUserResData(user);
  code = 200;
  
  return {code, token, userResData}
}



async function verifyUser(token : string) {

  let code = 500;
  const userId = jwt.read(token);

  await dbConn();
  const user : HydratedDocument<IUser> | null = 
    await User.findOne({_id: userId});
  if (!user) {
    code = 401;
    throw code
  }

  const userResData = toUserResData(user);
  code = 200

  return {code, userResData}
}