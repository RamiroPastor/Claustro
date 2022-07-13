import { serverSideTranslations } from 'next-i18next/serverSideTranslations';

import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { SignUp } from "/frontend/pages/User/SignUp/SignUp";



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}

export default function Handler(props) {
  
  return(
    <AuthGuard>
      <SignUp/>
    </AuthGuard>
  )
}