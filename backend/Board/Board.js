import mongoose from "mongoose";

import { config } from "centre/config/config.js";



const BoardSchema = new mongoose.Schema(
  { createdByUser:
    { type: mongoose.ObjectId
    , required: true
    }
  , title: 
    { type: String
    , unique: true   // this is not a validator
    , required: true
    , minLength: config.board.minLen_title
    , maxLength: config.board.maxLen_title
    , trim: true
    }
  , description: 
    { type: String
    , required: true
    , minLength: config.board.minLen_description
    , maxLength: config.board.maxLen_description
    , trim: true
    }
  , languages:
    { type: [String]
    , default: config.board.languages
    , enum: config.board.languages
    }
  , priority: 
    { type: Number
    }
  , archived:
    { type: Boolean
    }
  },
  { collection: "Boards"
  , timestamps: true
  }
)


export const Board = mongoose.models.Board || mongoose.model('Board', BoardSchema)


