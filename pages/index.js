import { useContext } from "react"
import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { AuthContext } from "frontend/core/contexts/AuthContext"
import { SignIn } from "frontend/pages/User/SignIn/SignIn"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}



export default function Home() {

  const isUserAuthenticated = useContext(AuthContext).isUserAuthenticated;

  return (
    ! isUserAuthenticated()
    ? <SignIn/>
    : <h1>Estas conectado</h1>
  )
}
