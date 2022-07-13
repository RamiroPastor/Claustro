import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { AuthGuard } from "frontend/pages/User/AuthGuard/AuthGuard"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}



export default function Home() {

  return (
    <AuthGuard>
      <h1>ESTAS ¡¡¡CONECTADO!!!</h1>
    </AuthGuard>
  )
}
