import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { threadController } from "backend/Thread/controllerThread"
import { AuthGuard } from "frontend/core/layout/AuthGuard/AuthGuard"
import { ThreadEdit } from "frontend/pages/Thread/ThreadEdit/ThreadEdit"



export async function getServerSideProps({locale, query}) {

  const translations = await serverSideTranslations(locale, ["common"])

  const threadList = await threadController.listThreads([query.threadId]);
  const thread = threadList.map(x => JSON.parse(x))[0];
  
  return ({ props: {...translations, thread}})
}



export default function Handler(props) {

  
  return(
    <AuthGuard>
      <ThreadEdit
        thread={props.thread}
      />
    </AuthGuard>
  )
}