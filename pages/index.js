import { useContext } from "react"
import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { UserContext } from "frontend/core/contexts/UserContext"
import { SignIn } from "frontend/pages/User/SignIn/SignIn"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}



export default function Home() {

  const user = useContext(UserContext).user;

  return (
    (user.jwt === null)
    ? <SignIn/>
    : <h1>Estas conectado como {user.name}</h1>
  )
}
