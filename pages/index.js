import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Forum     } from "frontend/pages/Forum/Forum"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}



export default function Home() {

  return (
    <AuthGuard>
      <Forum/>
    </AuthGuard>
  )
}
