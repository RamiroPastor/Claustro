import { serverSideTranslations } from 'next-i18next/serverSideTranslations';

import { SignIn } from "/frontend/pages/User/SignIn/SignIn";



export async function getStaticProps({locale}) {
  const translations = await serverSideTranslations(locale, ["common"])
  return({ props: {...translations}})
}

export default function Handler(props) {
  return(
    <SignIn/>
  )
}