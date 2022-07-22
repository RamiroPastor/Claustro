import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { boardController } from "backend/Board/controllerBoard"
import { threadController } from "backend/Thread/controllerThread"
import { communityController} from "backend/User/controllerCommunity"
import { sortThreads      } from "centre/Thread/sortThreads"
import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Forum     } from "frontend/pages/Forum/Forum"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  let boardList = await boardController.listBoards([])
  let threadList = await threadController.listThreads([])
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
