import { HydratedDocument, Types } from "mongoose"

import { dbConn } from "backend/base/dbConn"
import { BoardCreateData } from "centre/Board/BoardCreateData"
import { toBoardResData  } from "centre/Board/BoardResData"
import { BoardUpdateData } from "centre/Board/BoardUpdateData"
import { IBoard, Board } from "./Board"

export { boardController }




const boardController =
  { registerBoard
  , updateBoard
  , listBoards
  }



async function registerBoard(data : BoardCreateData) {

  let code : number = 500;

  await dbConn();

  const newBoard : HydratedDocument<IBoard> = new Board(
    { createdByUser: data.createdByUser
    , title        : data.title
    , description  : data.description
    , languages    : data.languages
    , priority     : 1
    , archived     : false
    }
  );

  const board = await newBoard.save();
  const boardResData = toBoardResData(board)
  code = 200;
  
  return {code, boardResData}
}


async function updateBoard(data : BoardUpdateData) {

  let code : number = 500;

  await dbConn();

  const boardBeforeUpdate : HydratedDocument<IBoard> | null= 
    await Board.findByIdAndUpdate(
      data.boardId,
      { title      : data.title
      , description: data.description
      , languages  : data.languages
      }
    );

  if (!boardBeforeUpdate) {
    code = 404;
    throw code;
  }
  code = 200;
  
  return {code}
}


async function listBoards(idList : String[]){

  await dbConn();

  let boardList : HydratedDocument<IBoard>[]

  if (idList.length === 0) {
    boardList = await Board.find();
  } else {
    boardList = 
      await Board.find()
                .where("_id").in(idList)
  }

  const resList = boardList.map(toBoardResData)
  
  return resList
}