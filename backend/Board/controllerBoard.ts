import mongoose from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { Board } from "./Board"

export { boardController }




const boardController =
  { registerBoard
  , listBoards
  }



async function registerBoard(userId : mongoose.ObjectId, title : String, desc : String, langs : [String]) {

  let code : number = 500;

  await dbConn();

  const newBoard = new Board(
    { createdByUser: userId
    , title: title
    , description: desc
    , languages: langs
    , priority: 1
    , archived: false
    }
  );

  const board = await newBoard.save();
  code = 200;
  
  return {code, board}
}


async function listBoards(idList : String[]){

  await dbConn();

  let boardList = []

  if (idList.length === 0) {
    boardList = await Board.find();
  } else {
    boardList = 
      await Board.find()
                .where("_id").in(idList)
  }
  
  return boardList.map(x => JSON.stringify(x))
}