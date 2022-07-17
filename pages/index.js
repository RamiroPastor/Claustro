import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { boardController } from "backend/Board/controllerBoard"
import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Forum     } from "frontend/pages/Forum/Forum"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  let boardList = await boardController.listBoards([])
  boardList = boardList.map(x => JSON.parse(x))
  return ({ props: {...translations, boardList}})
}



export default function Home(props) {

  return (
    <AuthGuard>
      <Forum
        boardList = {props.boardList}
      />
    </AuthGuard>
  )
}
