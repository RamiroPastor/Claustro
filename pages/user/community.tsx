import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { communityController } from "backend/User/controllerCommunity"
import { UserResData } from 'centre/User/UserResData'
import { AuthGuard } from "frontend/core/layout/AuthGuard/AuthGuard"
import { Community } from "frontend/pages/User/Community/Community"



export async function getServerSideProps({locale} : {locale: string}) {
  const translations = await serverSideTranslations(locale, ["common"])
  let userList = await communityController.listUsers([])
  return ({ props: {...translations, userList}})
}



export default function Handler(
    props : {userList : UserResData[]}
  ): JSX.Element {
  return(
    <AuthGuard>
      <Community userList={props.userList}/>
    </AuthGuard>
  )
}