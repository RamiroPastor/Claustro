import mongoose from "mongoose";

import { config } from "centre/config/config.js";



const ThreadSchema = new mongoose.Schema(
  { createdByUser:
    { type: mongoose.ObjectId
    , required: true
    }
  , boardId:
    { type: mongoose.ObjectId
    , required: true
    }
  , title: 
    { type: String
    , unique: true   // this is not a validator
    , required: true
    , minLength: config.thread.minLen_title
    , maxLength: config.thread.maxLen_title
    , trim: true
    }
  , description: 
    { type: String
    , required: true
    , minLength: config.thread.minLen_description
    , maxLength: config.thread.maxLen_description
    , trim: true
    }
  , pinned:
    { type: Boolean
    , default: false
    }
  , locked:
    { type: Boolean
    , default: false
    }
  , lastActivity:
    { type: {userId: mongoose.ObjectId, date: Date}
    , required: true
    }
  },
  { collection: "Threads"
  , timestamps: true
  }
)


export const Thread = mongoose.models.Thread || mongoose.model('Thread', ThreadSchema)


