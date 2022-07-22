import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { boardController } from "backend/Board/controllerBoard"
import { threadController } from "backend/Thread/controllerThread"
import { sortThreads      } from "backend/Thread/sortThreads"
import { communityController} from "backend/User/controllerCommunity"
import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Forum     } from "frontend/pages/Forum/Forum"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  let boardList = await boardController.listBoards([])
  let threadList = await threadController.listThreads([])
  threadList = threadList.map(x => JSON.parse(x))
  sortThreads(threadList)
  let userList = await communityController.listUsers([])
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
