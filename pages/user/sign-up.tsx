import { serverSideTranslations } from 'next-i18next/serverSideTranslations';

import { AuthGuard } from "frontend/core/layout/AuthGuard/AuthGuard"
import { SignUp } from "frontend/pages/User/SignUp/SignUp";



export async function getStaticProps({locale} : {locale: string}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}

export default function Handler() {
  
  return(
    <AuthGuard>
      <SignUp/>
    </AuthGuard>
  )
}