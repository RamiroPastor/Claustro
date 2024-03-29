import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { boardController } from "backend/Board/controllerBoard"
import { threadController } from "backend/Thread/controllerThread"
import { communityController} from "backend/User/controllerCommunity"
import { BoardResData } from 'centre/Board/BoardResData'
import { sortThreads   } from "centre/Thread/sortThreads"
import { ThreadResData } from 'centre/Thread/ThreadResData'
import { UserResData } from 'centre/User/UserResData'
import { AuthGuard } from "frontend/core/layout/AuthGuard/AuthGuard"
import { Forum     } from "frontend/pages/Forum/Forum"



export async function getServerSideProps({locale} : {locale: string}) {
  const translations = await serverSideTranslations(locale, ["common"])
  let boardList = await boardController.listBoards([])
  let threadList = await threadController.listThreads([])
  sortThreads(threadList)
  let userList = await communityController.listUsers([])
  return ({ props: {...translations, boardList, threadList, userList}})
}



export default function Home(
  props:
    { boardList  : BoardResData[]
    , threadList : ThreadResData[]
    , userList   : UserResData[]
    }
  ) {

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
