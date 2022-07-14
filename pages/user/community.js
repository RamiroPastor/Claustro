import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { communityController } from "backend/User/controllerCommunity"
import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Community } from "frontend/pages/User/Community/Community"



export async function getServerSideProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  let userList = await communityController.listUsers([])
  userList = userList.map(x => JSON.parse(x))
  return ({ props: {...translations, userList}})
}



export default function Handler(props) {
  
  return(
    <AuthGuard>
      <Community userList={props.userList}/>
    </AuthGuard>
  )
}