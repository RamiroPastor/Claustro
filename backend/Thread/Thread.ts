import mongoose from "mongoose";

import { config } from "centre/config/config.js";


export interface IThread 
  { createdByUser : mongoose.Types.ObjectId
  , boardId       : mongoose.Types.ObjectId
  , title         : string
  , description   : string
  , pinned        : number
  , locked        : boolean
  , lastActivity  : 
    { userId : mongoose.Types.ObjectId
    , date   : Date
    }
  , createdAt     : Date
  , updatedAt     : Date
  }


const ThreadSchema = new mongoose.Schema<IThread>(
  { createdByUser:
    { type: mongoose.Schema.Types.ObjectId
    , required: true
    }
  , boardId:
    { type: mongoose.Schema.Types.ObjectId
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
    { type: Number
    , default: 0
    , min: 0
    }
  , locked:
    { type: Boolean
    , default: false
    }
  , lastActivity:
    { type: {userId: mongoose.Schema.Types.ObjectId, date: Date}
    , required: true
    }
  },
  { collection: "Threads"
  , timestamps: true
  }
)


export const Thread = mongoose.models.Thread || mongoose.model<IThread>('Thread', ThreadSchema)


