import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { threadController } from "backend/Thread/controllerThread"
import { ThreadResData } from "centre/Thread/ThreadResData"
import { AuthGuard } from "frontend/core/layout/AuthGuard/AuthGuard"
import { ThreadEdit } from "frontend/pages/Thread/ThreadEdit/ThreadEdit"



export async function getServerSideProps(
  props: 
    { locale : string
    , query  : any
    }
  ) {

  const threadId = props.query.threadId as string;

  const translations = await serverSideTranslations(props.locale, ["common"])

  const threadList = await threadController.listThreads([threadId]);
  const thread = threadList[0];
  
  return ({ props: {...translations, thread}})
}



export default function Handler(
  props:
    { thread : ThreadResData
    }
  ) {

  
  return(
    <AuthGuard>
      <ThreadEdit
        thread={props.thread}
      />
    </AuthGuard>
  )
}