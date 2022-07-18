import mongoose from "mongoose";

import { config } from "centre/config/config.js";



const PostSchema = new mongoose.Schema(
  { userId:
    { type: mongoose.ObjectId
    , required: true
    }
  , threadId:
    { type: mongoose.ObjectId
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
    { type: [{user: mongoose.ObjectId, body: String, date: Date}]
    , default: []
    }
  },
  { collection: "Posts"
  , timestamps: true
  }
)


export const Post = mongoose.models.Post || mongoose.model('Post', PostSchema)


