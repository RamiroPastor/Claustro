import { serverSideTranslations } from 'next-i18next/serverSideTranslations';

import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Community } from "frontend/pages/User/Community/Community"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}

export default function Handler(props) {
  
  return(
    <AuthGuard>
      <Community/>
    </AuthGuard>
  )
}