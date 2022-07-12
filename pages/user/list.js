import { serverSideTranslations } from 'next-i18next/serverSideTranslations';

import { UserList } from "frontend/pages/User/UserList/UserList"



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}

export default function Handler(props) {
  return(
    <UserList/>
  )
}