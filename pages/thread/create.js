import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { AuthGuard    } from "frontend/core/layout/AuthGuard/AuthGuard"
import { ThreadCreate } from "frontend/pages/Thread/ThreadCreate/ThreadCreate"



export async function getServerSideProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return ({ props: {...translations}})
}



export default function Handler(props) {
  
  return(
    <AuthGuard>
      <ThreadCreate/>
    </AuthGuard>
  )
}