import mongoose from "mongoose";

import { config } from "centre/config/config.js";


export interface IUser 
  { name      : string
  , email     : string
  , password  : string
  , posts     : number
  , picture   : string
  , createdAt : Date
  , updatedAt : Date
  }


const UserSchema = new mongoose.Schema<IUser>(
  { name: 
    { type: String
    , required: true
    , minLength: config.user.minLen_name
    , maxLength: config.user.maxLen_name
    , trim: true
    // , match: /^[a-z\d](?:[a-z\d]|-(?=[a-z\d]))*$/i
    }
  , email: 
    { type: String
    , unique: true   // this is not a validator
    , required: true
    , minLength: config.user.minLen_email
    , maxLength: config.user.maxLen_email
    , trim: true
    , match: /^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*$/
    }
  , password: 
    { type: String
    , required: true
    }
  , posts:
    { type: Number
    , min: 0
    }
  , picture: 
    { type: String
    }
  },
  { collection: "Users"
  , timestamps: true
  }
)


export const User = mongoose.models.User || mongoose.model<IUser>('User', UserSchema)


