import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { boardController } from "backend/Board/controllerBoard"
import { postController } from "backend/Post/controllerPost"
import { threadController } from "backend/Thread/controllerThread"
import { communityController } from "backend/User/controllerCommunity"
import { BoardResData } from "centre/Board/BoardResData"
import { PostResData } from "centre/Post/PostResData"
import { ThreadResData } from "centre/Thread/ThreadResData"
import { UserResData } from "centre/User/UserResData"
import { AuthGuard } from "frontend/core/layout/AuthGuard/AuthGuard"
import { Thread } from "frontend/pages/Thread/Thread"



export async function getServerSideProps(
  props: 
    { locale : string
    , params : any
    }
  ) {

  const threadId = props.params.id as string;

  const translations = await serverSideTranslations(props.locale, ["common"])

  const threadList = await threadController.listThreads([threadId]);
  const thread = threadList[0];

  const boardList = await boardController.listBoards([thread.boardId]);
  const board = boardList[0];

  let postList = await postController.listThreadPosts(thread._id)

  const userIds = postList.map(p => p.userId);
  const uniqueUserIds = Array.from(new Set(userIds));
  let userList = await communityController.listUsers(uniqueUserIds);
  
  return ({ props: {...translations, board, thread, postList, userList}})
}



export default function Handler(
  props:
    { board    : BoardResData
    , thread   : ThreadResData
    , postList : PostResData[]
    , userList : UserResData[]
    }
  ) {
  
  return(
    <AuthGuard>
      <Thread
        board={props.board}
        thread={props.thread}
        postList={props.postList}
        userList={props.userList}
      />
    </AuthGuard>
  )
}