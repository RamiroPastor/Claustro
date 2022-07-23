import mongoose from "mongoose";

import { config } from "centre/config/config.js";




export interface IPostEdit
  { userId : mongoose.Types.ObjectId
  , body   : string
  , date   : Date
  }


export interface IPost 
  { userId      : mongoose.Types.ObjectId
  , threadId    : mongoose.Types.ObjectId
  , body        : string
  , editHistory : IPostEdit[]
  , createdAt   : Date
  , updatedAt   : Date
  }


const PostSchema = new mongoose.Schema<IPost>(
  { userId:
    { type: mongoose.Schema.Types.ObjectId
    , required: true
    }
  , threadId:
    { type: mongoose.Schema.Types.ObjectId
    , required: true
    }
  , body: 
    { type: String
    , unique: true   // this is not a validator
    , required: true
    , minLength: config.post.minLen
    , maxLength: config.post.maxLen
    , trim: true
    }
  , editHistory:
    { type: [
      { userId: mongoose.Schema.Types.ObjectId
      , body  : String
      , date  : Date
      }
    ]
    , default: []
    }
  },
  { collection: "Posts"
  , timestamps: true
  }
)


export const Post = mongoose.models.Post || mongoose.model<IPost>('Post', PostSchema)


