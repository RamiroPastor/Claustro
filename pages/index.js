import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Board     } from "frontend/pages/Board/Board"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}



export default function Home() {

  return (
    <AuthGuard>
      <Board/>
    </AuthGuard>
  )
}
