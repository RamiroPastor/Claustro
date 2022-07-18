import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { boardController } from "backend/Board/controllerBoard"
import { threadController } from "backend/Thread/controllerThread"
import { communityController} from "backend/User/controllerCommunity"
import { sort_newerFirstWithPrio } from 'centre/utils/sort'
import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Forum     } from "frontend/pages/Forum/Forum"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  let boardList = await boardController.listBoards([])
  boardList = boardList.map(x => JSON.parse(x))
  let threadList = await threadController.listThreads([])
  threadList = threadList.map(x => JSON.parse(x))
  sort_newerFirstWithPrio(threadList, "createdAt", "pinned")
  let userList = await communityController.listUsers([])
  userList = userList.map(x => JSON.parse(x))
  return ({ props: {...translations, boardList, threadList, userList}})
}



export default function Home(props) {

  return (
    <AuthGuard>
      <Forum
        boardList = {props.boardList}
        threadList = {props.threadList}
        userList = {props.userList}
      />
    </AuthGuard>
  )
}
